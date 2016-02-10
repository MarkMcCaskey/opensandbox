{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Logger
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Logger
  ( Logger
  , Lvl (..)
  , newLogger
  , writeTo
  , defaultBufSize
  ) where

import Control.Concurrent.STM.TVar
import Data.Monoid
import Data.Thyme.Format
import Data.Thyme.LocalTime
import System.Locale
import System.Log.FastLogger


data Lvl
  = Debug
  | Info
  | Notice
  | Warning
  | Err
  | Crit
  | Alert
  | Emerg
  deriving (Show,Eq,Enum,Ord)


instance ToLogStr Lvl where
  toLogStr = toLogStr . show


data Logger = Logger
  { loggerSet   :: LoggerSet
  , loggerLvl   :: (TVar Lvl)
  }


newLogger :: BufSize -> FilePath -> Lvl -> IO Logger
newLogger buf path lvl = do
  l <- newFileLoggerSet buf path
  newLvl <- newTVarIO lvl
  return $ Logger l newLvl


writeTo :: Logger -> Lvl -> String -> IO ()
writeTo logger lvl s = do
  minSeverity <- readTVarIO $ loggerLvl logger
  if (fromEnum minSeverity) <= (fromEnum lvl)
    then do
      time <- getZonedTime
      let timestampField = "["++ formatTime defaultTimeLocale "%T" time ++ "]"
      let lvlField = "[" ++ show lvl ++ "]"
      let msg = s
      let logEntry = timestampField <> " " <> lvlField <> " " <> msg
      putStrLn logEntry >> pushLogStrLn (loggerSet logger) (toLogStr logEntry)
    else
      return ()
