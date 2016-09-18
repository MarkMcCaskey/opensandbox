module OpenSandbox.Time
  ( tick
  , getWorldAge
  , getWorldTime
  ) where

import Control.Concurrent
import Control.Monad
import Data.Int

tick :: MVar Int64 -> IO ()
tick srvTicks = forever $ do
  threadDelay 50000
  modifyMVar_ srvTicks (\t -> return (t+1))

getWorldAge :: MVar Int64 -> IO Int64
getWorldAge = readMVar

getWorldTime :: MVar Int64 -> IO Int64
getWorldTime ticks = readMVar ticks >>= (\t -> return $ mod t 24000)
