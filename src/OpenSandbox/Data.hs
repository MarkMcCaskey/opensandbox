-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data
  ( module OpenSandbox.Data.Biome
  , module OpenSandbox.Data.Block
  , module OpenSandbox.Data.Effect
  , module OpenSandbox.Data.Entity
  , module OpenSandbox.Data.Instrument
  , module OpenSandbox.Data.Item
  , module OpenSandbox.Data.Window
  , module OpenSandbox.Data.Yggdrasil
  , GameData (..)
  , loadGameData
  ) where

import OpenSandbox.Data.Biome
import OpenSandbox.Data.Block
import OpenSandbox.Data.Effect
import OpenSandbox.Data.Entity
import OpenSandbox.Data.Instrument
import OpenSandbox.Data.Item
import OpenSandbox.Data.Window
import OpenSandbox.Data.Yggdrasil

import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString as B
import Data.Map.Strict
import Data.Text

data GameData = GameData
  { getBiomeMap :: Map Text Biome
  , getBlockMap :: Map Text Block
  , getEffectMap :: Map Text Effect
  , getEntityMap :: Map Text Entity
  , getInstrumentMap :: Map Text Instrument
  , getItemMap :: Map Text Item
  , getWindowMap :: Map Text Window
  } deriving (Show,Eq)


loadGameData :: IO (Either String GameData)
loadGameData = do
  rawBiomes <- B.readFile "minecraft-data/data/pc/1.10/biomes.json"
  rawBlocks <- B.readFile "minecraft-data/data/pc/1.10/blocks.json"
  rawEffects <- B.readFile "minecraft-data/data/pc/1.10/effects.json"
  rawEntity <- B.readFile "minecraft-data/data/pc/1.10/entities.json"
  rawInstruments <- B.readFile "minecraft-data/data/pc/1.10/instruments.json"
  rawItems <- B.readFile "minecraft-data/data/pc/1.10/items.json"
  rawWindows <- B.readFile "minecraft-data/data/pc/1.10/windows.json"

  let biomes = eitherDecodeStrict' rawBiomes :: Either String [Biome]
  let blocks = eitherDecodeStrict' rawBlocks :: Either String [Block]
  let effects = eitherDecodeStrict' rawEffects :: Either String [Effect]
  let entities = eitherDecodeStrict' rawEntity :: Either String [Entity]
  let instruments = eitherDecodeStrict' rawInstruments :: Either String [Instrument]
  let items = eitherDecodeStrict' rawItems :: Either String [Item]
  let windows = eitherDecodeStrict' rawWindows :: Either String [Window]

  let biomeMap = fmap genBiomeMap biomes
  let blockMap = fmap genBlockMap blocks
  let effectMap = fmap genEffectMap effects
  let entityMap = fmap genEntityMap entities
  let instrumentMap = fmap genInstrumentMap instruments
  let itemMap = fmap genItemMap items
  let windowMap = fmap genWindowMap windows

  return $
    GameData
    <$> biomeMap
    <*> blockMap
    <*> effectMap
    <*> entityMap
    <*> instrumentMap
    <*> itemMap
    <*> windowMap
