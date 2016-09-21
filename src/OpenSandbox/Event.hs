module OpenSandbox.Event
  ( Event (..)
  , Command (..)
  ) where

import qualified Data.Text as T
import Data.Int
import qualified Data.Vector as V

data Event = Event Int64 Command deriving (Show,Eq)

data Command
  = PlayerPositionAndLook Double Double Double Float Float Bool
  | TabComplete (V.Vector T.Text)
  | ChatMessage T.Text Int8
  deriving (Show,Eq)
