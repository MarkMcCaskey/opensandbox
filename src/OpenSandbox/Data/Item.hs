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
module OpenSandbox.Data.Item
  ( Item (..)
  , genItemMap
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Data
import qualified Data.Map.Strict as Map
import Data.Text as T
import Data.Word
import GHC.Generics (Generic)
import Prelude hiding (id)

data Item = Item
  { id            :: Word32
  , displayName   :: Text
  , stackSize     :: Word8
  , name          :: Text
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON Item
instance FromJSON Item
instance NFData Item

genItemMap :: [Item] -> Map.Map Text Item
genItemMap = Map.fromList . fmap (\i -> (name i,i))
