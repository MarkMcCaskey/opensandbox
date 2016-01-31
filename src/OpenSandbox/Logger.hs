{-# LANGUAGE OverloadedStrings #-}
module OpenSandbox.Logger
  ( Lvl (..)
  , writeTo
  ) where

import Data.Monoid
import Data.Thyme.Format
import Data.Thyme.LocalTime
import System.Locale
import System.Log.FastLogger

data Lvl = Info | Warn | Error | Debug deriving (Show,Eq)

instance ToLogStr Lvl where
  toLogStr = toLogStr . show

writeTo :: LoggerSet -> Lvl -> String -> IO ()
writeTo logger lvl s = do
  time <- getZonedTime
  let timestampField = toLogStr $ "["++ formatTime defaultTimeLocale "%T" time ++ "]"
  let lvlField = toLogStr $ "[" ++ show lvl ++ "]"
  let msg = toLogStr s
  pushLogStrLn logger $ timestampField <> " " <> lvlField <> " " <> msg
