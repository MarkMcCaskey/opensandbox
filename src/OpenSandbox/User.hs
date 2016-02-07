module OpenSandbox.User
  ( User (..)
  , UserCache
  ) where

import qualified  Data.Map as Map
import qualified  Data.Text as T
import            Data.UUID

type UserCache = Map.Map T.Text User


data User = User
  { userName    :: T.Text
  , userUUID    :: UUID
  } deriving (Show,Eq)
