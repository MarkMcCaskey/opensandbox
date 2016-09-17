{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data.Block
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data.Block
  ( Palette
  , genGlobalPalette
  , Block (..)
  , genBlockMap
  , BlockStateID
  , BlockIndice
  ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as A
import Data.Bits
import Data.Data
import Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Scientific
import Data.Serialize
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word
import Control.DeepSeq
import Foreign.Storable
import GHC.Generics (Generic)
import Prelude hiding (id)

genBlockMap :: [Block] -> Map.Map T.Text Block
genBlockMap = Map.fromList . fmap (\b -> (name b, b))

newtype BlockStateID = BlockStateID Word16
  deriving (Show,Eq,Ord,Enum,Bounded,Bits,Num,Real,Integral,Storable,Generic,Hashable)

instance Serialize BlockStateID where
  put (BlockStateID bid) = putWord16be bid
  get = BlockStateID <$> getWord16be

instance NFData BlockStateID

newtype BlockIndice = BlockIndice Word16
  deriving (Show,Eq,Ord,Enum,Bits,Bounded,Num,Real,Integral,Generic)

instance Serialize BlockIndice where
  put (BlockIndice indice) = putWord16be indice
  get = BlockIndice <$> getWord16be

instance NFData BlockIndice

data Block = Block
  { id            :: Word16
  , displayName   :: T.Text
  , name          :: T.Text
  , hardness      :: Double
  , stackSize     :: Word8
  , diggable      :: Bool
  , material      :: Maybe Material
  , harvestTools  :: Maybe [Word32]
  , variations    :: Maybe [Variation]
  , drops         :: [Drop]
  , transparent   :: Bool
  , emitLight     :: Word8
  , filterLight   :: Word8
  } deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON Block where
  parseJSON (Object v) = Block
      <$> v .: "id"
      <*> v .: "displayName"
      <*> v .: "name"
      <*> v .: "hardness"
      <*> v .: "stackSize"
      <*> v .: "diggable"
      <*> v .:? "material"
      <*> (fmap . fmap) extractIds (v .:? "harvestTools" :: Parser (Maybe Object))
      <*> v .:? "variations"
      <*> v .: "drops"
      <*> v .: "transparent"
      <*> v .: "emitLight"
      <*> v .: "filterLight"
    where
    extractIds obj = case sequence (fmap extractId (H.keys obj)) of
                      Left err -> fail err
                      Right lst -> lst
    extractId x = A.parseOnly A.decimal x :: Either String Word32
  parseJSON x = typeMismatch "Error: Invalid BlockImport!" x

instance NFData Block

data Material
  = MaterialRock
  | MaterialDirt
  | MaterialWood
  | MaterialPlant
  | MaterialLeaves
  | MaterialWeb
  | MaterialWool
  deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON Material where
  parseJSON (String s) =
    case s of
      "rock" -> return MaterialRock
      "dirt" -> return MaterialDirt
      "wood" -> return MaterialWood
      "plant" -> return MaterialPlant
      "leaves" -> return MaterialLeaves
      "web" -> return MaterialWeb
      "wool" -> return MaterialWool
      x       -> fail $ "ERROR => Aeson => failed to pattern match text to Material: " ++ show x

  parseJSON x = typeMismatch ("ERROR => Aeson => Material => not a String, got " ++ show x) x

instance NFData Material

data Drop = Drop
  { drop      :: DropEntry
  , minCount  :: Maybe Word8
  , maxCount  :: Maybe Word8
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance FromJSON Drop
instance NFData Drop

newtype DropEntry = DropEntry (Either Word32 DropBody)
  deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance FromJSON DropEntry where
  parseJSON (Number n) = return $ DropEntry . Left . toEnum . base10Exponent $ n
  parseJSON (Object o) = fmap (DropEntry . Right) $ DropBody
    <$> o .: "id"
    <*> o .: "metadata"
  parseJSON x = typeMismatch "Error: Invalid DropEntry!" x

instance NFData DropEntry

data DropBody = DropBody
  { id        :: Word32
  , metadata  :: Word32
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance FromJSON DropBody
instance NFData DropBody

data Variation = Variation
  { metadata      :: Word16
  , displayName   :: T.Text
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON Variation
instance FromJSON Variation
instance NFData Variation

type Palette = V.Vector BlockStateID

genGlobalPalette :: [Block] -> Palette
genGlobalPalette = V.fromList . L.sort . L.concatMap encodeBlockImport
  where
    getBlockImportId :: Block -> Word16
    getBlockImportId = id
    getMetadata :: Variation -> Word16
    getMetadata = metadata
    encodedBlockID :: Block -> Word16
    encodedBlockID bi = (fromIntegral $ getBlockImportId bi) `shiftL` 4
    encodeBlockImport :: Block -> [BlockStateID]
    encodeBlockImport bi =
      case variations bi of
        Nothing -> [BlockStateID $ toEnum . fromEnum $ encodedBlockID bi]
        Just vlst -> fmap (BlockStateID . toEnum . fromEnum . (\x -> encodedBlockID bi .|. x) . toEnum . fromEnum . getMetadata) vlst
