{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data.Item
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data.Window
  ( Window (..)
  , WindowSlot (..)
  , WindowProperty (..)
  , FurnaceProperty (..)
  , EnchantmentTableProperty (..)
  , BeaconProperty (..)
  , AnvilProperty (..)
  , BrewingStandProperty (..)
  , genWindowMap
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types
import Data.Data
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Text
import GHC.Generics (Generic)
import Prelude hiding (id)

data Window = Window
  { id :: Text
  , name :: Text
  , slots :: Maybe [WindowSlot]
  , properties :: Maybe [Text]
  , openWith :: Maybe [Text]
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON Window
instance FromJSON Window
instance NFData Window

genWindowMap :: [Window] -> Map.Map Text Window
genWindowMap = Map.fromList . (fmap (\w -> (winName w, w)))
  where
    winName :: Window -> Text
    winName (Window _ n _ _ _) = n

data WindowSlot = WindowSlot
  { index :: Int
  , name :: Text
  , size :: Maybe Int
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON WindowSlot
instance FromJSON WindowSlot
instance NFData WindowSlot

data WindowProperty
  = WindowFurnace FurnaceProperty
  | WindowEnchantmentTable EnchantmentTableProperty
  | WindowBeacon BeaconProperty
  | WindowAnvil AnvilProperty
  | WindowBrewingStand BrewingStandProperty
  deriving (Show,Eq,Ord,Generic)

data FurnaceProperty
  = FireIcon
  | MaxBurnTime
  | ProgressArrow
  | MaxProgress
  deriving (Show,Eq,Ord,Enum,Generic)

data EnchantmentTableProperty
  = LvlReqTopSlot
  | LvlReqMiddleSlot
  | LvlReqBottomSlot
  | Seed
  | TopMouseHover
  | MiddleMouseHover
  | BottomMouseHover
  deriving (Show,Eq,Ord,Enum,Generic)

data BeaconProperty
  = PowerLevel
  | FirstPotionEffect
  | SecondPotionEffect
  deriving (Show,Eq,Ord,Enum,Generic)

data AnvilProperty
  = RepairCost
  deriving (Show,Eq,Ord,Enum,Generic)

data BrewingStandProperty
  = BrewTime
  deriving (Show,Eq,Ord,Enum,Generic)

data OpenWith = OpenWith
  { openWithtype :: Text
  , openWithId :: Int
  } deriving (Show,Eq)

instance ToJSON OpenWith where
  toJSON (OpenWith a b) =
    object
      [ "type" .= a
      , "id" .= b
      ]

instance FromJSON OpenWith where
  parseJSON (Object v) = OpenWith
      <$> v .: "type"
      <*> v .: "id"

  parseJSON x = typeMismatch "Error: Invalid OpenWith!" x
