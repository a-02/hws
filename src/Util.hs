{-# OPTIONS -fglasgow-exts #-}
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

module Util where

import System.Posix
import Control.Exception
import Control.Concurrent
import Network.Socket

import Time
import System.Time
import Locale
import Char
import IO
import Data.Bits
import List
import Maybe

-- XXX necessary?
import GHC.Base
import GHC.Conc
import GHC.IOBase

#ifdef DEBUG
import qualified Debug.Trace
#endif

-----------------------------------------------------------------------------
-- Utils

#ifdef DEBUG
trace s e = Debug.Trace.trace s e
#else
trace s e = e
#endif
traceVal v = trace (show v) v

crlf = "\r\n"

emptyLine "\r" = True
emptyLine _    = False

stripWS :: String -> String
stripWS = stripLeadingWS . reverse . stripLeadingWS . reverse

stripLeadingWS :: String -> String
stripLeadingWS = dropWhile isSpace

data E b a = Ok a | Bad b
instance Monad (E b) where
   m >>= k = case m of
	        Ok  a -> k a
		Bad b -> Bad b
   return a = Ok a

failE :: b -> E b a
failE b = Bad b

maybeE :: b -> Maybe a -> E b a
maybeE _ (Just a) = return a
maybeE b Nothing  = failE b

commaSep :: String -> [String]
commaSep s = go (dropWhile isSpace s)
  where go "" = []
	go s  = word : case rest of ',':rest -> go rest; _ -> go rest
	  where (word,rest) = break (==',') s

-- ToDo: deHex is supposed to remove the '%'-encoding
deHex :: String -> String
deHex s = s

hPutStrCrLf h s = hPutStr h s >> hPutChar h '\r' >> hPutChar h '\n'

-----------------------------------------------------------------------------
-- Time utils

formatTimeSensibly :: CalendarTime -> String
formatTimeSensibly time
   = formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" time

epochTimeToClockTime :: EpochTime -> ClockTime
epochTimeToClockTime epoch_time = TOD (fromIntegral (fromEnum epoch_time)) 0

-----------------------------------------------------------------------------
-- concurrency utilities

-- timeout

-- Time-outs are implemented by having another thread wait for the
-- specified period of time before sending an exception to the
-- original thread.  We have to be extremely careful about race
-- conditions here: we don't want the timeout thread raising an
-- exception outside of our handler, so we must arrange that the
-- timeout exception can only be raised when we're ready for it.  This
-- is implemented using a semaphore to indicate that the thread is
-- ready to handle the timeout exception.
--
-- Things get hairy when we consider that the action being run may
-- generate its own exceptions.

timeout
   :: Int	-- secs
   -> IO a	-- action to run
   -> IO a	-- action to run on timeout
   -> IO a

timeout secs action on_timeout 
  = do
    threadid <- myThreadId
    timeout <- forkIOIgnoreExceptions (
			    do threadDelay (secs * 1000000)
			       throwTo threadid (ErrorCall "__timeout")
			  )
    ( do result <- action
	 killThread timeout
	 return result
      ) 
      `Control.Exception.catch`
      ( \exception -> case exception of
		       ErrorCall "__timeout" -> on_timeout		       
		       _other                -> do
						killThread timeout
						throw exception )

fromHex :: Char -> Int
fromHex c | (c >= '0' && c <= '9') = (fromEnum c) - (fromEnum '0')
          | (c >= 'a' && c <= 'f') = (fromEnum c) - (fromEnum 'a') + 10
          | (c >= 'A' && c <= 'F') = (fromEnum c) - (fromEnum 'A') + 10
          | otherwise = -1
toHex :: Int -> Char
toHex n = "0123456789ABCDEF" !! (n .&. 0xf)

urlDecode :: String -> String
urlDecode [] = []
urlDecode ('%':a:b:xs) = if (av /= -1) && (bv /= -1)
			   then (toEnum (av * 16 + bv)) : urlDecode xs
			   else '%' : a : b : urlDecode xs where
			 av = fromHex a
			 bv = fromHex b
urlDecode (x:xs) = x : urlDecode xs

urlEncode :: String -> String
urlEncode = concatMap transChars where
    transChars c | (c >= 'a' && c <= 'z') = return c
                 | (c >= 'A' && c <= 'Z') = return c
                 | (c >= '0' && c <= '9') = return c
                 | isJust (elemIndex c "$-_.+!*'()/") = return c
		 | otherwise = '%' : (toHex (cv `shiftR` 4)) : (toHex cv) : [] where
		     cv = fromEnum c

splitStr ch s = case span (/= ch) s of
                    (x, []) -> x : []
                    (x, ch : xs) -> x : splitStr ch xs
joinStr ch [] = []
joinStr ch (x:[]) = x
joinStr ch (x:xs) = x ++ (ch : (joinStr ch xs))

{- Strip out ".." components in path names, and colons (dangerous in win32) -}
pathSep = '/'
normPath = rootNull . (joinStr pathSep) . unColon . (normDotDot []) . (splitStr pathSep) . urlDecode where
    --normDotDot ps ("":xs) = normDotDot ps xs
    normDotDot (p:ps) ("..":xs) = normDotDot ps xs
    normDotDot ps ("..":xs) = normDotDot ps xs
    normDotDot ps (x:xs) = normDotDot (x:ps) xs
    normDotDot ps [] = reverse ps
    unColon ps = map (filter (/= ':')) ps
    rootNull [] = "/"
    rootNull xs = xs

forkIOIgnoreExceptions :: IO () -> IO ThreadId
forkIOIgnoreExceptions action = IO $ \ s -> 
   case (fork# action s) of (# s1, id #) -> (# s1, ThreadId id #)

-----------------------------------------------------------------------------
-- networking utils

accept :: Socket 		-- Listening Socket
       -> IO (Handle,SockAddr)	-- StdIO Handle for read/write
accept sock = do
 (sock', addr) <- Network.Socket.accept sock
 handle	<- socketToHandle sock' ReadWriteMode
 return (handle,addr)

