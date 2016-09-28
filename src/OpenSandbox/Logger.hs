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

import Control.Concurrent (ThreadId,forkIO)
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Logger
import Data.Monoid
import qualified Data.Text as T
import System.Log.FastLogger

data Logger = Logger
  { _lInbox :: Chan (Loc, LogSource, LogLevel, LogStr)
  , _lTimeCache :: IO FormattedTime
  , _lSpec :: FileLogSpec
  , _lLvl :: Lvl
  }

newLogger :: FileLogSpec -> Lvl -> IO Logger
newLogger spec lvl = do
  chan <- newChan
  timeCache <- newTimeCache timeFormat
  return $ Logger chan timeCache spec lvl

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

logIO :: Logger -> T.Text -> Lvl -> T.Text -> IO ()
logIO logger src lvl msg = when (lvl >= (_lLvl logger)) $ runChanLoggingT (_lInbox logger) $ logFrom src lvl msg

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

level :: LogLevel -> LogStr
level LevelDebug = "DEBUG"
level LevelInfo = "INFO"
level LevelWarn = "WARNING"
level LevelError = "ERR"
level (LevelOther lvl) = toLogStr lvl

customLogStr :: FormattedTime -> Loc -> LogSource -> LogLevel -> LogStr -> LogStr
customLogStr t _ b c d = "[" <> toLogStr t <> " " <> toLogStr b <> "/" <> level c <> "]" <> " " <> d <> "\n"

customOutput :: LogType -> IO FormattedTime -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
customOutput logType timeCache loc src lvl msg = do
  t <- timeCache
  withFastLogger logType (customLogger (ls t))
  withFastLogger (LogStdout 0) (customLogger (ls t))
  where ls t = customLogStr t loc src lvl msg
        customLogger :: LogStr -> (LogStr -> IO ()) -> IO ()
        customLogger str f = f str

runCustomLoggingT :: FileLogSpec -> BufSize -> IO FormattedTime -> LoggingT IO () -> IO ()
runCustomLoggingT spec buf t l = (l `runLoggingT` (customOutput (LogFile spec buf) t))

runLogger :: Logger -> IO ThreadId
runLogger (Logger chan timeCache spec _) =
  forkIO $ runCustomLoggingT spec 100000 timeCache $ unChanLoggingT chan
