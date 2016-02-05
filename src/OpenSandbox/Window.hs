-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Window
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Window
  ( Window (..)
  , Slot (..)
  , OpenWith (..)
  , OpenWithType (..)
  ) where

import qualified  Data.Text as T

data Window = Window
  { windowID          :: !T.Text
  , windowName        :: !T.Text
  , windowSlots       :: !(Maybe [Slot])
  , windowProperties  :: !(Maybe [T.Text])
  , windowOpenWith  :: !(Maybe OpenWith)
  } deriving (Show,Eq)


data Slot = Slot
  { slotName    :: !T.Text
  , slotIndex   :: !Int
  , slotSize    :: !(Maybe Int)
  } deriving (Show,Eq)


data OpenWithType = OpenWithBlock | OpenWithEntity deriving (Show,Eq)


data OpenWith = OpenWith
  { openWithType  :: !OpenWithType
  , openWithID    :: !Int
  } deriving (Show,Eq)
