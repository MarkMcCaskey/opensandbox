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
  ) where

import          Control.DeepSeq
import          Data.Aeson
import          Data.Data
import          Data.Typeable
import          Data.Text as T
import          Data.Word
import          GHC.Generics (Generic)
import          Prelude hiding (id)

data Item = Item
  { id            :: Word32
  , displayName   :: Text
  , stackSize     :: Word8
  , name          :: Text
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON Item
instance FromJSON Item

instance NFData Item
