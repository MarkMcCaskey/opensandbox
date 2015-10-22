-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.Log
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Log
    ( LogEntry
    , EntryType
    , Message
    , Log
    , readLogFile
    , writeLogFile
    ) where


import Data.Time


data LogEntry = LogEntry
  { entryTime    :: TimeOfDay
  , entryType    :: EntryType
  , entryMessage :: Message
  } deriving (Show,Eq,Ord)


data EntryType = Info | Warn | Error deriving (Show,Eq,Ord)


type Message = String


type Log = [LogEntry]


readLogFile :: FilePath -> IO (Either String Log)
readLogFile path = undefined


writeLogFile :: FilePath -> Log -> IO ()
writeLogFile path log = undefined
