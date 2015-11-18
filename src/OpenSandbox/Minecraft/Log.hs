{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.Log
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Log
    ( LogEntry
    , Log
    , logEntryParser
    , logParser
    , loadLogFiles
    , javaWarningParser
    , javaWarningBodyParser
    , gameErrorParser
    ) where

import qualified  Codec.Compression.GZip as Gzip
import            Control.Applicative
import            Data.Time
import            Data.Attoparsec.ByteString.Char8
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.List as List
import            OpenSandbox.Minecraft.User
import            Prelude hiding (takeWhile)
import            System.Directory


data LogEntry = LogEntry
  { _entryTime           :: TimeOfDay
  , _entrySubsystem      :: Subsystem
  , _entryProcessType    :: ProcessType
  , _entrySeverityLevel  :: SeverityLevel
  , _entryMessage        :: Message
  } deriving (Show,Eq,Ord)


data SeverityLevel
  = Info
  | Warn
  | Error
  deriving (Show,Eq,Ord)


data Subsystem
  = Server
  | ServerShutdown
  | UserAuth
  deriving (Show,Eq,Ord)


data ProcessType
  = Thread
  | Auth Int
  deriving (Show,Eq,Ord)


type Message = B.ByteString


type JavaError = [B.ByteString]

type Error = [B.ByteString]

type Log = [(Either JavaError LogEntry)]


-- | Parser for the timestamp of a log entry.
timeStampParser :: Parser TimeOfDay
timeStampParser = do
    h <- count 2 digit
    _ <- char ':'
    m <- count 2 digit
    _ <- char ':'
    s <- count 2 digit
    return $ TimeOfDay (read h) (read m) (read s)


-- | Parser for the subsystem that a log entry is from.
subsystemParser :: Parser Subsystem
subsystemParser =
        (string "Server Shutdown"     >> return ServerShutdown)
    <|> (string "Server"              >> return Server)
    <|> (string "User Authenticator"  >> return UserAuth)


-- | Parser for the source the log entry is from.
processTypeParser :: Parser ProcessType
processTypeParser =
        (string "thread"   >> return Thread)
    <|> (string "Thread"   >> return Thread)
    <|> do  char '#'
            x <- decimal
            return $ Auth x


-- | Parser for the severity level of the log entry.
severityLevelParser :: Parser SeverityLevel
severityLevelParser =
        (string "INFO"  >> return Info)
    <|> (string "WARN"  >> return Warn)
    <|> (string "ERROR" >> return Error)


-- | Parser for a log entry, one line in a log file
logEntryParser :: Parser (Either JavaError LogEntry)
logEntryParser = do
    _ <- char '['
    t <- timeStampParser
    _ <- char ']'
    _ <- space
    _ <- char '['
    s <- subsystemParser
    _ <- space
    p <- processTypeParser
    _ <- char '/'
    l <- severityLevelParser
    _ <- char ']'
    _ <- char ':'
    _ <- space
    m <- takeWhile (/='\n') <* endOfLine
    return $ Right $ LogEntry t s p l m


-- | Parser for java errors thrown in the log file
javaWarningParser :: Parser (Either JavaError LogEntry)
javaWarningParser = do
    _ <- string "java"
    x <- takeWhile (/='\n') <* endOfLine
    xs <- manyTill javaWarningBodyParser logEntryParser
    return $ Left ("java":x:xs)


-- | Parser for each additional line in the java error
javaWarningBodyParser :: Parser B.ByteString
javaWarningBodyParser = do
    _ <- char '\t'
    takeWhile (/='\n') <* endOfLine


-- | Parser for game errors. Basically, we won't bother parsing it in detail,
-- because we have no idea what we might get.
gameErrorParser :: Parser (Either Error LogEntry)
gameErrorParser = do
    xs <- manyTill' gameErrorBodyParser logEntryParser
    return $ Left xs


gameErrorBodyParser :: Parser B.ByteString
gameErrorBodyParser = takeWhile (/='\n') <* endOfLine


-- | Parser for an entire Minecraft server log file
logParser :: Parser Log
logParser = many $ choice [logEntryParser,javaWarningParser,gameErrorParser]


loadLogFiles :: FilePath -> IO B.ByteString
loadLogFiles path = do
    files <- getDirectoryContents path
    let fileList = List.drop 2 $ List.sort files
    let latest = List.last fileList
    let archived = List.init fileList
    raw <- mapM BL.readFile $ map (path ++) archived
    return $ B.concat $ fmap (BL.toStrict . Gzip.decompress) raw
