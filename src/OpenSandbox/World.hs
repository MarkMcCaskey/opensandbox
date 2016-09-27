{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.World
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.World
  ( module OpenSandbox.World.Chunk
  , module OpenSandbox.World.Flat
  , World
  , WorldType (..)
  , newWorld
  , genWorld
  , pullWorld
  ) where

import OpenSandbox.World.Chunk
import OpenSandbox.World.Flat

import Control.Concurrent.STM
import Data.Int
import qualified Data.Map.Lazy as ML

import OpenSandbox.Config
import OpenSandbox.Event

data World = World
  { _wInbox :: TQueue WorldCommand
  , _wOutbox :: TQueue Event
  , _worldStorage :: WorldStorage
  }

data WorldCommand

type WorldStorage = ML.Map (Int32,Int32) ChunkColumn

newWorld :: WorldType -> Config -> STM (Either String World)
newWorld worldType config = do
  inbox <- newTQueue
  outbox <- newTQueue
  let eitherStorage = genWorld worldType config
  case eitherStorage of
    Left err -> return $ Left err
    Right storage -> return $ Right $
      World inbox outbox storage

genWorld :: WorldType -> Config -> Either String WorldStorage
genWorld worldType config =
  case worldType of
    Default -> Left "Error: No implemented world gen for Default!"
    Flat -> ML.fromList <$> ((\layers -> genFlatWorld layers (srvViewDistance config)) =<< (mkChunkLayers classicFlatPreset))
    LargeBiomes -> Left "Error: No implemented world gen for LargeBiomes!"
    Amplified -> Left "Error: No implemented world gen for Amplified!"

pullWorld :: World -> [ChunkColumn]
pullWorld (World _ _ storage)= ML.elems storage
