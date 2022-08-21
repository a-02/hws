-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

module Main (main) where

import MimeTypes
import Request
import Response
import ErrorLogger
import AccessLogger
import ConfigParser
import Config hiding (listen)
import Util

import System
import Network.URI
import System.Posix
import System.Posix.Signals
import Network.BSD
import IO hiding (bracket)
import Control.Exception
import Monad
import Control.Concurrent
import Network.Socket hiding (accept)
import Foreign
import Foreign.C.Error
import Data.IORef
import System.Console.GetOpt
import Dir

{- -----------------------------------------------------------------------------
ToDo:

- MAJOR:

- deal with http version numbers
- timeouts (partly done)
- languages
- CGI scripts
- per-directory permissions (ala apache)
- error logging levels
- virtual hosts, per-directory config options.
- languages (content-language, accept-language)
- multipart/byteranges

- MINOR:

- access logging (various bits left)
- implement "listen" (listen on additional ports)
- implement user & group setting
- log time to serve request
- terminate & restart signal (like Apache's SIGHUP)
- don't die if the new configuration file contains errors after a restart
- reading config file may block, unsafe if we receive another SIGHUP
- common up headers with same name (eg. accept).
- implement if-modified-since (need to parse time)

- MAYBE:

- throttling if too many open connections (config: MaxClients)

----------------------------------------------------------------------------- -}
-- command-line options

data CmdLineOpt
  = O_ConfigFile String
  | O_ServerRoot String

options = [
  Option ['f'] ["config"] (ReqArg O_ConfigFile "filename") 
        "default: \"conf/httpd.conf\"",
  Option ['d'] ["server-root"]  (ReqArg O_ServerRoot "directory")
        "default: \"/etc/httpd\""
  ]

usage = "usage: hws [option...]"

cmdline :: IORef [CmdLineOpt]
cmdline = unsafePerformIO (newIORef (error "no flags"))

defaultConfigFile = "conf/httpd.conf"
defaultServerRoot = "/home/hws" -- "/etc/httpd"

-- ToDo: set this in main
serverRoot = unsafePerformIO $ do
  args <- readIORef cmdline
  case [ s | O_ServerRoot s <- args] of
        []    -> return defaultServerRoot
        (s:_) -> return s

configFile = unsafePerformIO $ do
  args <- readIORef cmdline
  case [ s | O_ConfigFile s <- args] of
        []    -> return defaultConfigFile
        (s:_) -> return s
  
configPath = serverRoot ++ '/':configFile

-----------------------------------------------------------------------------
-- Top-level server

main = do
  args <- getArgs
  case getOpt Permute options args of
     (flags, [], []) -> do 
         writeIORef cmdline flags
         main2
     (_, _, errs)    -> do 
         hPutStr stderr (concat errs)
         hPutStr stderr (usageInfo usage options)

main2 = do
  main_thread <- myThreadId
  installHandler sigPIPE Ignore Nothing
  installHandler sigHUP (Catch (hupHandler main_thread)) Nothing
  block $ readConfig

hupHandler :: ThreadId -> IO ()
hupHandler main_thread
  = throwTo main_thread (ErrorCall "**restart**")

sigsToBlock = addSignal sigHUP emptySignalSet

-- Async exceptions should be blocked on entry to readConfig (so that
-- multiple SIGHUPs close together can't kill us).  Make sure that
-- there aren't any interruptible operations until we've blocked signals.
readConfig = do
    blockSignals sigsToBlock
    r <- parseConfig configPath
    case r of
      Left err -> do
        hPutStrLn stderr ("failed to parse " ++ configPath)
        hPutStr stderr (show err)
      Right b  -> do
        let conf = b defaultConfig
        
        initMimeTypes (typesConfig conf)        -- read mime.types
        startErrorLogger  conf                  -- start the error log thread
        startAccessLogger conf                  -- start the logging thread
        
        my_hostent <- do                        -- get my hostname/address
           ent <- getHostEntry
           case serverName conf of              -- Replace the name if
                "" -> return ent                -- serverName is set.
                n  -> return ent{hostName = n}
        putMVar local_hostent my_hostent

        topServer conf

-- We catch exceptions from the main server thread, and restart the
-- server.  If we receive a restart signal (from a SIGHUP), then we
-- re-read the configuration file.
topServer conf
  = Control.Exception.catch
               (do unblockSignals sigsToBlock
                   unblock $ do
                   server conf)
        (\e -> case e of
                   ErrorCall "**restart**" -> do
                        takeMVar local_hostent
                        stopAccessLogger 
                        stopErrorLogger
                        readConfig
                   IOException io -> do
                        logError ("server: " ++ showIOError io)
                        topServer conf
                   _other -> do 
                        logError ("server: " ++ show e)
                        topServer conf
        )

-- open the server socket and start accepting connections
server conf = do
  proto <- getProtocolNumber "tcp"
  Control.Exception.bracket
     (socket AF_INET Stream proto)
     (\sock -> sClose sock)
     (\sock -> do
        setSocketOption sock ReuseAddr 1
        bindSocket sock (SockAddrInet (fromIntegral (port conf)) iNADDR_ANY)
        listen sock maxListenQueue
        acceptConnections conf sock
    )

-- accept connections, and fork off a new thread to handle each one
acceptConnections conf sock = do
  (h, SockAddrInet port haddr) <- accept sock
  forkIO ( (talk conf h haddr  `finally`  (hClose h))
            `Control.Exception.catch` 
          (\e -> trace ("servlet died: "  ++ show e) (return  ()))
        )
  acceptConnections conf sock


talk conf h haddr = do
  hSetBuffering h LineBuffering
  run conf True h haddr


run conf first h haddr = do
    -- read a request up to the first empty line.  If we
    -- don't get a request within the alloted time, issue
    -- a "Request Time-out" response and close the connection.
    let time_allowed | first     = requestTimeout conf
                     | otherwise = keepAliveTimeout conf

    trace "Reading request..." $ do
    req <- catchJust ioErrors (
            timeout time_allowed
              (do r <- getRequest h
                  return (Just r))
              (do -- only send a "request timed out" response if this
                  -- was the first request on the socket.  Subsequent
                  -- requests time-out and close the socket silently.
                  -- ToDo: if we get a partial request, still emit the
                  -- the timeout response.
                  (if first
                        then response conf h (requestTimeOutResponse conf)
                        else return ())
                  return Nothing)
           )
           (\e@io -> 
                if isEOFError e
                     then trace "EOF from client" $ return Nothing
                     else do logError ("request: " ++ showIOError io) 
                             return Nothing )

    case req of { Nothing -> return ();  Just r ->
    trace "Got request" $ do

    -- tmp: dump out the request
#ifdef DEBUG
    mapM_ (hPutStrLn stderr) r
#endif

    case parseRequest r of

         -- close the connection after a badly formatted request
         Bad resp -> do 
              trace (show (resp conf)) $ do
              response conf h (resp conf)
              return ()

         Ok  req  -> do 
              resp <- request conf req
              trace (show resp) $ do
              logAccess req resp haddr (error "noTimeDiff"){-ToDo-}
              response conf h resp

              -- Persistent Connections
              --
              -- We close the connection if
              --   (a) client specified "connection: close"
              --   (b) client is pre-HTTP/1.1, and didn't
              --       specify "connection: keep-alive"

              let connection_headers = getConnection (reqHeaders req)
              if ConnectionClose `elem` connection_headers
                 || (reqHTTPVer req < http1_1
                     && ConnectionKeepAlive `notElem` connection_headers)
                   then return ()
                   else run conf False h haddr
   }

-- Read lines up to the first empty line
-- ToDo: handle LWS
getRequest :: Handle -> IO [String]
getRequest h = do
  l <- hGetLine h
  if (emptyLine l) 
     then getRequest h
     else getRequest' l h

getRequest' l h = do
  if (emptyLine l) 
     then return []
     else do l' <- hGetLine h
             ls <- getRequest' l' h
             return (l:ls)

-----------------------------------------------------------------------------
-- Dealing with requests

request :: Config -> Request -> IO Response
request conf req@Request{reqCmd = cmd}
  = ( -- make sure we've got a host field 
      -- if the request version is >= HTTP/1.1
      case checkHostHeader conf req of
         Just response -> return (response conf)
         Nothing -> case cmd of
                      GetReq  -> doGet conf req False
                      HeadReq -> doGet conf req True
                      _ -> return (notImplementedResponse conf)
    ) 
      `Control.Exception.catch`
    ( \exception -> do
         logError ("request: " ++ show exception)
         return (internalServerErrorResponse conf)
    )

checkHostHeader conf Request{reqHTTPVer = ver, reqHeaders = headers}
  = case getHost headers of
           [] | ver < http1_1                    -> Nothing
           [host] | host == serverName conf 
                  || host `elem` serverAlias conf -> Nothing
                  | otherwise                    -> Just notFoundResponse
           _                                     -> Just badRequestResponse

doGet conf Request{reqURI = uri, reqHeaders = headers} is_head
 = case uri of
      NoURI          -> return (badRequestResponse conf)
      AuthorityURI _ -> return (badRequestResponse conf)
      AbsPath path   -> getFile conf path is_head headers
      AbsURI uri     ->
        if (uriScheme uri /= "http") 
           -- ToDo: || (authority uri /= host)
           || (uriQuery uri /= "")
           || (uriFragment uri /= "")
           then return (notFoundResponse conf) 
           else getFile conf (uriPath uri) is_head headers

getFile conf path is_head headers = do
   let npath = normPath path            -- XXX perhaps better to error if path has colons
   m_path <- prependDocRoot conf npath
   case m_path of {
        Left r -> return r;
        Right path -> do

   check <- findRealFilename conf npath path
   case check of { 
       Left r -> return r;
       Right (filename,stat) -> do
   
           -- check we can actually read this file
   access <- fileAccess filename True{-read-} False False
   case access of {
       False -> return (notFoundResponse conf);
                   -- not "permission denied", we're being paranoid
                   -- about security.
       True -> do
   
   let last_modified = 
         lastModifiedHeader (epochTimeToClockTime (modificationTime stat))
   let send_body = not is_head

   -- XXX here would be a good place to generalize
   --   dir listings
   --   cgis
   (body,content_type) <- 
        if isDirectory stat
             then createDirListing npath filename
             else
                let size = toInteger (fileSize stat)
                    content_type = 
                        case mimeTypeOf filename of
                            Nothing -> contentTypeHeader (show (defaultType conf))
                            Just t  -> contentTypeHeader (show t) in
                return ((FileBody size filename),content_type) 

   return (okResponse conf body
              [content_type, last_modified]
              send_body)
   }}}

statFile :: String -> IO (Maybe FileStatus)
statFile filename = do
  maybe_stat <- tryJust ioErrors (getFileStatus filename)
  case maybe_stat of
       Left e -> do
          errno <- getErrno
          if errno == eNOENT
             then return Nothing
             else ioError e
       Right stat ->
          return (Just stat)

-- expand "~user" in the path name
prependDocRoot :: Config -> String -> IO (Either Response String)
prependDocRoot conf ('/':'~':userpath) | not (null (userDir conf)) = do
  let (user, path) = break (=='/') userpath
  u_ent <- tryJust ioErrors (getUserEntryForName user)
  case u_ent of
    Left _    -> return (Left (notFoundResponse conf))
    Right ent -> return (Right ('/': homeDirectory ent ++ 
                                '/':userDir conf ++ path))
prependDocRoot conf path@('/':_) = do
  return (Right (documentRoot conf ++ path))
prependDocRoot conf _path = return (Left (notFoundResponse conf))

-- See if the path exists, and if it implicitely references index.html.
-- If its a directory without a trailing slash, redirect appropriately.
findRealFilename :: Config -> String -> String -> IO (Either Response (String,FileStatus))
findRealFilename conf urlpath filename = do
  stat <- statFile filename
  case stat of
      Nothing -> return (Left (notFoundResponse conf))
      Just stat
         | isDirectory stat -> 
             if (last urlpath == '/') then do
                let index_filename = filename ++ '/': directoryIndex conf
                indexstat <- statFile index_filename
                case indexstat of
                   Nothing -> return (Right (filename,stat))
                   Just stat -> return (Right (index_filename,stat))
             else 
                let dirurl = urlpath ++ "/" in
                return (Left (movedPermanentlyResponse conf dirurl))
         | isRegularFile stat ->
             return (Right (filename,stat))
         | otherwise ->
             return (Left (notFoundResponse conf))

-- ignore port for now
getHost hdrs = [ h | Host h p <- hdrs ]
getConnection hdrs = [ c | Connection cs <- hdrs, c <- cs ]

