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
  , EntityType (..)
  , EntityCategory (..)
  ) where

import          Control.DeepSeq
import          Data.Aeson
import          Data.Aeson.Types
import          Data.Data
import          Data.Monoid
import          Data.Text as T
import          Data.Word
import          GHC.Generics (Generic)
import          Prelude hiding (id)

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
  = EntityCat_Drops
  | EntityCat_Immobile
  | EntityCat_Projectiles
  | EntityCat_Blocks
  | EntityCat_Vehicles
  | EntityCat_NPCs
  | EntityCat_HostileMobs
  | EntityCat_PassiveMobs
  | EntityCat_Generic
  deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON EntityCategory where
  toJSON ec =
    case ec of
      EntityCat_Drops       -> String "Drops"
      EntityCat_Immobile    -> String "Immobile"
      EntityCat_Projectiles -> String "Projectiles"
      EntityCat_Blocks      -> String "Blocks"
      EntityCat_Vehicles    -> String "Vehicles"
      EntityCat_NPCs        -> String "NPCs"
      EntityCat_HostileMobs -> String "Hostile mobs"
      EntityCat_PassiveMobs -> String "Passive mobs"
      EntityCat_Generic     -> String "Generic"

instance FromJSON EntityCategory where
  parseJSON (String s) =
    case s of
      "Drops"         -> return EntityCat_Drops
      "Immobile"      -> return EntityCat_Immobile
      "Projectiles"   -> return EntityCat_Projectiles
      "Blocks"        -> return EntityCat_Blocks
      "Vehicles"      -> return EntityCat_Vehicles
      "NPCs"          -> return EntityCat_NPCs
      "Hostile mobs"  -> return EntityCat_HostileMobs
      "Passive mobs"  -> return EntityCat_PassiveMobs
      "Generic"       -> return EntityCat_Generic
      _               -> undefined

  parseJSON x = typeMismatch "Error: invalid JSON for EntityCategory!" x
        
instance NFData EntityCategory