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
  , userNamespace
  , insertUser
  , deleteUser
  ) where

import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Data.UUID
import Data.UUID.V5

type UserStore = Map.Map UUID User

data User = User
  { getUserUUID     :: UUID
  , getUserName     :: T.Text
  } deriving (Show,Eq)

userNamespace :: UUID
userNamespace = fromJust.fromText $ "edf9fc48-9df1-4091-8751-93a660223eb6"

insertUser :: UserStore -> User -> UserStore
insertUser store user =
    Map.insert (generateNamed userNamespace . B.unpack . encodeUtf8 $ (getUserName user)) user store

deleteUser :: UserStore -> T.Text -> UserStore
deleteUser store username = Map.filter ((/=) username . getUserName) store
