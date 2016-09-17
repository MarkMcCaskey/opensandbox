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
import OpenSandbox.Data.Yggdrasil

import Data.Aeson (FromJSON,eitherDecodeStrict')
import qualified Data.ByteString as B
import Data.Map.Strict
import Data.Text
import Path

data GameData = GameData
  { biomeMap :: Map Text Biome
  , globalPalette :: Palette
  , effectMap :: Map Text Effect
  , entityMap :: Map Text Entity
  , instrumentMap :: Map Text Instrument
  , itemMap :: Map Text Item
  } deriving (Show,Eq)


loadGameData :: IO (Either String GameData)
loadGameData = do
  rawBiomes <- B.readFile "data/biomes.json"
  rawBlocks <- B.readFile "data/blocks.json"
  rawEffects <- B.readFile "data/effects.json"
  rawEntity <- B.readFile "data/entities.json"
  rawInstruments <- B.readFile "data/instruments.json"
  rawItems <- B.readFile "data/items.json"

  let biomes = eitherDecodeStrict' rawBiomes :: Either String [Biome]
  let blocks = eitherDecodeStrict' rawBlocks :: Either String [BlockImport]
  let effects = eitherDecodeStrict' rawEffects :: Either String [Effect]
  let entities = eitherDecodeStrict' rawEntity :: Either String [Entity]
  let instruments = eitherDecodeStrict' rawInstruments :: Either String [Instrument]
  let items = eitherDecodeStrict' rawItems :: Either String [Item]

  let biomeMap = fmap genBiomeMap biomes
  let globalPalette = fmap genGlobalPalette blocks
  let effectMap = fmap genEffectMap effects
  let entityMap = fmap genEntityMap entities
  let instrumentMap = fmap genInstrumentMap instruments
  let itemMap = fmap genItemMap items

  return $ GameData <$> biomeMap <*> globalPalette <*> effectMap <*> entityMap <*> instrumentMap <*> itemMap
