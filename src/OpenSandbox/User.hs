{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.User
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.User
  ( User (..)
  , UserStore
  , registerUser
  ) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as MS
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4

type UserStore = MS.Map T.Text User

data User = User
  { getUserUUID     :: UUID
  , getUserName     :: T.Text
  } deriving (Show,Eq)

registerUser :: TVar UserStore -> T.Text -> IO User
registerUser existingUsers username = do
  existingUsers' <- readTVarIO existingUsers
  case MS.lookup username existingUsers' of
    Nothing -> do
      someUUID <- nextRandom
      let newUser = User someUUID username
      atomically . writeTVar existingUsers $
        MS.insert username newUser existingUsers'
      return newUser
    Just user -> return user
