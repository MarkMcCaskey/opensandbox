{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data.Entity
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data.Entity
  ( Entity (..)
  , genEntityMap
  , EntityType (..)
  , EntityCategory (..)
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types
import Data.Data
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Text as T
import Data.Word
import GHC.Generics (Generic)
import Prelude hiding (id)

data Entity = Entity
  { id            :: Word32
  , internalId    :: Maybe Word32
  , name          :: Text
  , displayName   :: Text
  , entityType    :: EntityType
  , width         :: Double
  , height        :: Double
  , category      :: Maybe EntityCategory
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON Entity where
  toJSON (Entity a b c d e f g h) =
    object
      [ "id" .= a
      , "internalId" .= b
      , "name" .= c
      , "displayName" .= d
      , "entityType" .= e
      , "width" .= f
      , "height" .= g
      , "category" .= h
      ]

instance FromJSON Entity where
  parseJSON (Object v) =
    Entity
      <$> v .: "id"
      <*> v .:? "internalId"
      <*> v .: "name"
      <*> v .: "displayName"
      <*> v .: "type"
      <*> v .: "width"
      <*> v .: "height"
      <*> v .:? "category"

  parseJSON _ = mempty

instance NFData Entity

genEntityMap :: [Entity] -> Map.Map Text Entity
genEntityMap = Map.fromList . fmap (\e -> (name e, e))

data EntityType = EntityMob | EntityObject
  deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON EntityType where
  toJSON et =
    case et of
      EntityMob     -> String "mob"
      EntityObject  -> String "object"

instance FromJSON EntityType where
  parseJSON (String s) =
    case s of
      "mob" -> return EntityMob
      "object" -> return EntityObject
      _ -> undefined

  parseJSON x = typeMismatch "Error: invalid JSON for EntityType!" x

instance NFData EntityType

data EntityCategory
  = EntityCatDrops
  | EntityCatImmobile
  | EntityCatProjectiles
  | EntityCatBlocks
  | EntityCatVehicles
  | EntityCatNPCs
  | EntityCatHostileMobs
  | EntityCatPassiveMobs
  | EntityCatGeneric
  deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON EntityCategory where
  toJSON ec =
    case ec of
      EntityCatDrops       -> String "Drops"
      EntityCatImmobile    -> String "Immobile"
      EntityCatProjectiles -> String "Projectiles"
      EntityCatBlocks      -> String "Blocks"
      EntityCatVehicles    -> String "Vehicles"
      EntityCatNPCs        -> String "NPCs"
      EntityCatHostileMobs -> String "Hostile mobs"
      EntityCatPassiveMobs -> String "Passive mobs"
      EntityCatGeneric     -> String "Generic"

instance FromJSON EntityCategory where
  parseJSON (String s) =
    case s of
      "Drops"         -> return EntityCatDrops
      "Immobile"      -> return EntityCatImmobile
      "Projectiles"   -> return EntityCatProjectiles
      "Blocks"        -> return EntityCatBlocks
      "Vehicles"      -> return EntityCatVehicles
      "NPCs"          -> return EntityCatNPCs
      "Hostile mobs"  -> return EntityCatHostileMobs
      "Passive mobs"  -> return EntityCatPassiveMobs
      "Generic"       -> return EntityCatGeneric
      _               -> undefined

  parseJSON x = typeMismatch "Error: invalid JSON for EntityCategory!" x

instance NFData EntityCategory
