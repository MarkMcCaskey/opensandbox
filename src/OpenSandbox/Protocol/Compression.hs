module OpenSandbox.Protocol.Compression
  ( Compression (..)
  ) where

import Data.Word

data Compression = Disabled | Everything | Threshold Word16
  deriving (Show,Eq)
