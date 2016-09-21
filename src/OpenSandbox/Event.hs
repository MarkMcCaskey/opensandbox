-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Event
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Event
  ( Event (..)
  , Command (..)
  ) where

import qualified Data.Text as T
import Data.Int

data Event = Event
  { getEventTick :: Int64
  , getEventCmd :: Command
  } deriving (Show,Eq)

data Command
  = PlayerPositionAndLook Double Double Double Float Float Bool
  | ChatMessage T.Text Int8
  deriving (Show,Eq)
