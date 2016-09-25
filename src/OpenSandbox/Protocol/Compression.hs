-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Compression
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Compression
  ( Compression (..)
  ) where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Serialize
import Data.Word

import OpenSandbox.Protocol.Types (putVarInt)

data Compression = Disabled | Everything | Threshold Word16
  deriving (Show,Eq)
