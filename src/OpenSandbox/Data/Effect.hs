{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data.Effect
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data.Effect
  ( Effect (..)
  , genEffectMap
  , EffectType (..)
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types
import Data.Data
import qualified Data.Map.Strict as Map
import Data.Text as T
import Data.Word
import GHC.Generics (Generic)
import Prelude hiding (id)

data Effect = Effect
  { id            :: Word32
  , name          :: Text
  , displayName   :: Text
  , effectType    :: EffectType
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON Effect where
  toJSON (Effect a b c d) =
    object
      [ "id" .= a
      , "name" .= b
      , "displayName" .= c
      , "type" .= d
      ]

instance FromJSON Effect where
  parseJSON (Object v) = Effect
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "displayName"
      <*> v .: "type"

  parseJSON x = typeMismatch "Error: Invalid Effect!" x

instance NFData Effect

genEffectMap :: [Effect] -> Map.Map Text Effect
genEffectMap = Map.fromList . fmap (\e -> (name e, e))

data EffectType = GoodEffect | BadEffect
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance ToJSON EffectType where
  toJSON GoodEffect = String "good"
  toJSON BadEffect = String "bad"

instance FromJSON EffectType where
  parseJSON (String s) =
    case s of
      "good"  -> return GoodEffect
      "bad"   -> return BadEffect
      _       -> undefined
  parseJSON x = typeMismatch "Error: Invalid EffectType!" x

instance NFData EffectType
