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
  ( MonadLogger
  , ToLogStr
  , Logger
  , Lvl (..)
  , FileLogSpec (..)
  , newLogger
  , logIO
  , logFrom
  , defaultBufSize
  , runLogger
  ) where

import            Control.Concurrent
import            Control.Concurrent.Chan
import            Control.Monad
import            Control.Monad.IO.Class
import            Control.Monad.Logger
import            Data.Monoid
import qualified  Data.Text as T
import            System.Log.FastLogger

data Logger = Logger
  { lChan       :: Chan (Loc, LogSource, LogLevel, LogStr)
  , lTimeCache  :: IO FormattedTime
  , lSpec       :: FileLogSpec
  }

newLogger :: FileLogSpec -> IO Logger
newLogger spec = do
  chan <- newChan
  timeCache <- newTimeCache timeFormat
  return $ Logger chan timeCache spec

data Lvl
  = LvlDebug
  | LvlInfo
  | LvlNotice
  | LvlWarning
  | LvlError
  | LvlCritical
  | LvlAlert
  | LvlEmergency
  deriving (Show,Eq,Ord,Enum)

instance ToLogStr Lvl where
  toLogStr = toLogStr . show

logIO :: MonadIO m => Logger -> T.Text -> Lvl -> T.Text -> m ()
logIO logger src lvl msg = runChanLoggingT (lChan logger) $ logFrom src lvl msg

logFrom :: MonadLogger m => T.Text -> Lvl -> T.Text -> m ()
logFrom src lvl msg = logOtherNS src convertedLvl msg
  where convertedLvl = case lvl of
                        LvlDebug -> LevelOther "DEBUG"
                        LvlInfo -> LevelOther "INFO"
                        LvlNotice -> LevelOther "NOTICE"
                        LvlWarning -> LevelOther "WARNING"
                        LvlError -> LevelOther "ERR"
                        LvlCritical -> LevelOther "CRIT"
                        LvlAlert -> LevelOther "ALERT"
                        LvlEmergency -> LevelOther "EMERG"

timeFormat :: TimeFormat
timeFormat = "%Y-%m-%d %T"

timestamp :: FormattedTime -> LogStr
timestamp time = toLogStr time

loc :: Loc -> LogStr
loc _ = mempty

source :: LogSource -> LogStr
source src = toLogStr src

level :: LogLevel -> LogStr
level LevelDebug = "DEBUG"
level LevelInfo = "INFO"
level LevelWarn = "WARNING"
level LevelError = "ERR"
level (LevelOther lvl) = toLogStr lvl

customLogStr :: FormattedTime -> Loc -> LogSource -> LogLevel -> LogStr -> LogStr
customLogStr t a b c d = "[" <> timestamp t <> " " <> source b <> "/" <> level c <> "]" <> " " <> d <> "\n"

customLogger :: LogStr -> (LogStr -> IO ()) -> IO ()
customLogger str f = f str

customOutput:: LogType -> IO FormattedTime -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
customOutput logType timeCache loc src lvl msg = do
  t <- timeCache
  withFastLogger logType (customLogger (ls t))
  withFastLogger (LogStdout 0) (customLogger (ls t))
  where ls t = customLogStr t loc src lvl msg

runCustomLoggingT :: MonadIO m => FileLogSpec -> BufSize -> IO FormattedTime -> LoggingT m a -> m a
runCustomLoggingT spec buf t l = (l `runLoggingT` (customOutput (LogFile spec buf) t))

runLogger :: Logger -> IO ()
runLogger (Logger chan timeCache spec) =
  void $ forkIO $ runCustomLoggingT spec 100000 timeCache $ unChanLoggingT chan
