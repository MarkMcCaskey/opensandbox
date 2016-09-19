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
  ( module OpenSandbox.World.Amplified
  , module OpenSandbox.World.Chunk
  , module OpenSandbox.World.Default
  , module OpenSandbox.World.Flat
  , module OpenSandbox.World.LargeBiomes
  , World
  , WorldType (..)
  , genWorld
  ) where

import OpenSandbox.World.Amplified
import OpenSandbox.World.Chunk
import OpenSandbox.World.Default
import OpenSandbox.World.Flat
import OpenSandbox.World.LargeBiomes

import Data.Int
import qualified Data.Map.Lazy as ML

import OpenSandbox.Config
import OpenSandbox.Logger

type World = ML.Map (Int32,Int32) ChunkColumn

genWorld :: WorldType -> Config -> Logger -> Either String World
genWorld worldType config _ =
  case worldType of
    Default -> Left "Error: No implemented world gen for Default!"
    Flat -> ML.fromList <$> ((\layers -> genFlatWorld layers (srvViewDistance config)) =<< (mkChunkLayers classicFlatPreset))
    LargeBiomes -> Left "Error: No implemented world gen for LargeBiomes!"
    Amplified -> Left "Error: No implemented world gen for Amplified!"
