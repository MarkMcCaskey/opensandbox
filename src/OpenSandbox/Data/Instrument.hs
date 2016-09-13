{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data.Instrument
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data.Instrument
  ( Instrument (..)
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Data
import Data.Text as T
import Data.Word
import GHC.Generics (Generic)
import Prelude hiding (id)

data Instrument = Instrument
  { id    :: Word32
  , name  :: Text
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON Instrument
instance FromJSON Instrument
instance NFData Instrument
