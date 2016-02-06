module OpenSandbox.User
  ( User (..)
  ) where

import qualified  Data.Map as Map
import qualified  Data.Text as T

data User = User
  { userUUID  :: UUID
  , realName  :: T.Text
  } deriving (Show,Eq)
