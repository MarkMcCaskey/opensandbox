module OpenSandbox.Event
  ( Event (..)
  , GameStateDiff (..)
  ) where

import Data.Int

data Event = Event Int64 GameStateDiff deriving (Show,Eq)

data GameStateDiff
  = PlayerPositionAndLookDiff Double Double Double Float Float Bool
  deriving (Show,Eq)
