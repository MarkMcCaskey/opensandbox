{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data.Biome
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data.Biome
  ( Biome (..)
  , BiomeID (..)
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Data
import Data.Scientific
import Data.Text as T
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import Prelude hiding (id)

data Biome = Biome
  { id              :: BiomeID
  , color           :: Word32
  , name            :: Text
  , rainfall        :: Double
  , temperature     :: Double
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON Biome
instance FromJSON Biome
instance NFData Biome

data BiomeID
  = BiomeIDOcean
  | BiomeIDPlains
  | BiomeIDDesert
  | BiomeIDExtremeHills
  | BiomeIDForest
  | BiomeIDTaiga
  | BiomeIDSwampland
  | BiomeIDRiver
  | BiomeIDHell
  | BiomeIDTheEnd
  | BiomeIDFrozenOcean
  | BiomeIDFrozenRiver
  | BiomeIDIcePlains
  | BiomeIDIceMountains
  | BiomeIDMushroomIsland
  | BiomeIDMushroomIslandShore
  | BiomeIDBeach
  | BiomeIDDesertHills
  | BiomeIDForestHills
  | BiomeIDTaigaHills
  | BiomeIDExtremeHillsEdge
  | BiomeIDJungle
  | BiomeIDJungleHills
  | BiomeIDJungleEdge
  | BiomeIDDeepOcean
  | BiomeIDStoneBeach
  | BiomeIDColdBeach
  | BiomeIDBirchForest
  | BiomeIDBirchForestHills
  | BiomeIDRoofedForest
  | BiomeIDColdTaiga
  | BiomeIDColdTaigaHills
  | BiomeIDMegaTaiga
  | BiomeIDMegaTaigaHills
  | BiomeIDExtremeHillsPlus
  | BiomeIDSavanna
  | BiomeIDSavannaPlateau
  | BiomeIDMesa
  | BiomeIDMesaPlateauF
  | BiomeIDMesaPlateau
  | BiomeIDTheVoid
  | BiomeIDPlainsM
  | BiomeIDSunflowerPlains
  | BiomeIDDesertM
  | BiomeIDExtremeHillsM
  | BiomeIDFlowerForest
  | BiomeIDTaigaM
  | BiomeIDSwamplandM
  | BiomeIDIcePlainsSpikes
  | BiomeIDJungleM
  | BiomeIDJungleEdgeM
  | BiomeIDBirchForestM
  | BiomeIDBirchForestHillsM
  | BiomeIDRoofedForestM
  | BiomeIDColdTaigaM
  | BiomeIDMegaSpruceTaiga
  | BiomeIDRedwoodTaigaHillsM
  | BiomeIDExtremeHillsPlusM
  | BiomeIDSavannaM
  | BiomeIDSavannaPlateauM
  | BiomeIDMesaBryce
  | BiomeIDMesaPlateauFM
  | BiomeIDMesaPlateauM
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance ToJSON BiomeID where
  toJSON b = toJSON (fromEnum b)

instance FromJSON BiomeID where
  parseJSON (Number n) = return (toEnum (base10Exponent n))

instance NFData BiomeID
