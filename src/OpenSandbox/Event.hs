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
  ) where

import qualified Data.Text as T
import Data.Int

type Tick = Int64

data Event
  = PlayerPositionAndLook Tick Double Double Double Float Float Bool
  | ChatMessage Tick T.Text Int8
  deriving (Show,Eq)
