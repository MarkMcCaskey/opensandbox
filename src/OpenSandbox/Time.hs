-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Time
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Time
  ( WorldClock
  , newWorldClock
  , tick
  , getWorldAge
  , getWorldTime
  ) where

import Control.Concurrent
import Control.Monad
import Data.Int

newtype WorldClock = WorldClock (MVar Int64)

newWorldClock :: IO WorldClock
newWorldClock = WorldClock <$> newMVar (0 :: Int64)

tick :: WorldClock -> IO ()
tick (WorldClock tickStore) = forever $ do
  threadDelay 50000
  modifyMVar_ tickStore (\t -> return (t+1))

getWorldAge :: WorldClock -> IO Int64
getWorldAge (WorldClock tickStore)= readMVar tickStore

getWorldTime :: WorldClock -> IO Int64
getWorldTime (WorldClock tickStore) = readMVar tickStore >>= (\t -> return $ mod t 24000)
