module OpenSandbox.Item ( Item (..) ) where
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Item
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------

import qualified  Data.ByteString as BC
import qualified  Data.Text as T

import OpenSandbox.Types

data Item = Item
  { itemID  :: Int
  , itemDisplayName   :: T.Text
  , itemName          :: BC.ByteString
  , itemStackSize     :: Int
  , itemVariations    :: Maybe [Variation]
  } deriving (Show,Eq)
