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

module ConfigParser where

import Parser
import ParseToken
import Config

type ConfigBuilder = Config -> Config

parseConfig :: String -> IO (Either ParseError ConfigBuilder)
parseConfig fname
  = parseFromFile configParser fname

configParser :: Parser ConfigBuilder
configParser = do
  whiteSpace
  cs <- many configLine
  eof
  return (foldr (.) id cs)

configLine :: Parser ConfigBuilder
configLine
 = do (reserved "user"                   >> p_user)
  <|> (reserved "group"                  >> p_group)
  <|> (reserved "timeout"                >> p_timeout)
  <|> (reserved "keepalivetimeout"       >> p_keepAliveTimeout)
  <|> (reserved "maxclients"             >> p_maxClients)
  <|> (reserved "port"                   >> p_port)
  <|> (reserved "serveradmin"            >> p_serverAdmin)
  <|> (reserved "servername"             >> p_serverName)
  <|> (reserved "serveralias"            >> p_serverAlias)
  <|> (reserved "usecanonicalname"       >> p_useCanonicalName)
  <|> (reserved "documentroot"           >> p_documentRoot)
  <|> (reserved "userdir"                >> p_userDir)
  <|> (reserved "directoryindex"         >> p_directoryIndex)
  <|> (reserved "accessfilename"         >> p_accessFileName)
  <|> (reserved "typesconfig"            >> p_typesConfig)
  <|> (reserved "defaulttype"            >> p_defaultType)
  <|> (reserved "hostnamelookups"        >> p_hostnameLookups)
  <|> (reserved "errorlog"               >> p_errorLog)
  <|> (reserved "loglevel"               >> p_logLevel)
  <|> (reserved "accesslogfile"          >> p_accessLogFile)
  <|> (reserved "accesslogformat"        >> p_accessLogFormat)
  <|> (reserved "listen"                 >> p_listen)
  <|> (reserved "addlanguage"            >> p_addlanguage)
  <|> (reserved "languagepriority"       >> p_languagepriority)

p_user  = do str <- stringLiteral; return (\c -> c{user = str})
p_group = do str <- stringLiteral; return (\c -> c{group = str})
p_timeout = do i <- int; return (\c -> c{requestTimeout = i})
p_keepAliveTimeout = do i <- int; return (\c -> c{keepAliveTimeout = i})
p_maxClients  = do i <- int; return (\c -> c{maxClients = i})
p_port = do i <- int; return (\c -> c{port = i})
p_serverAdmin = do str <- stringLiteral; return (\c -> c{serverAdmin = str})
p_serverName = do str <- stringLiteral; return (\c -> c{serverName = str})
p_serverAlias = do str <- stringLiteral
		   return (\c -> c{serverAlias = str : serverAlias c})
p_useCanonicalName = do b <- bool; return (\c -> c{useCanonicalName = b})
p_documentRoot = do str <- stringLiteral; return (\c -> c{documentRoot = str})
p_userDir = do str <- stringLiteral; return (\c -> c{userDir = str})
p_directoryIndex = do str <- stringLiteral; return (\c -> c{directoryIndex = str})
p_accessFileName = do str <- stringLiteral; return (\c -> c{accessFileName = str})
p_typesConfig = do str <- stringLiteral; return (\c -> c{typesConfig = str})
p_defaultType = do str <- stringLiteral; return (\c -> c{defaultType = str})
p_hostnameLookups = do b <- bool; return (\c -> c{hostnameLookups = b})
p_errorLog = do str <- stringLiteral; return (\c -> c{errorLogFile = str})
p_logLevel = do i <- int; return (\c -> c{logLevel = i})
p_accessLogFile = do str <- stringLiteral; return (\c -> c{accessLogFile = str})
p_accessLogFormat = do str <- stringLiteral; return (\c -> c{accessLogFormat = str})
p_listen = do i <- int; return (\c -> c{listen = i : listen c})
p_addlanguage = do lang <- stringLiteral; ext <- stringLiteral; return (\c -> c{addLanguage = (lang,ext) : addLanguage c})
p_languagepriority = do langs <- many stringLiteral; return (\c -> c{languagePriority = langs})

bool = do { reserved "On"; return True } 
   <|> do { reserved "Off"; return False }

int :: Parser Int
int = do i <- integer; return (fromInteger i)

