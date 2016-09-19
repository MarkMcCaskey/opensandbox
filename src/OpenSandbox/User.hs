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
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.UUID

type UserStore = M.Map T.Text User

data User = User
  { getUserUUID     :: UUID
  , getUserName     :: T.Text
  } deriving (Show,Eq)
