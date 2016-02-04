-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Entity
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Entity
  ( Entity (..)
  , EntityType (..)
  , EntityCatagory (..)
  ) where

import qualified  Data.Text as T

data EntityType
  = Mob
  | Object
  deriving (Show,Eq)


data EntityCatagory
  = Generic
  | HostileEntity
  | PassiveEntity
  | NPCEntity
  | VehicleEntity
  | DropEntity
  | BlockEntity
  | ImmobileEntity
  | ProjectileEntity
  deriving (Show,Eq)


data Entity = Entity
  { entityID            :: !Int
  , entityInternalID    :: !Int
  , entityName          :: !T.Text
  , entityDisplayName   :: !T.Text
  , entityType          :: !EntityType
  , entityWidth         :: !(Maybe Int)
  , entityHeight        :: !(Maybe Int)
  , entityCatagory      :: !EntityCatagory
  } deriving (Show,Eq)
