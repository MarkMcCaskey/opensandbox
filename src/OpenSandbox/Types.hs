{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Types
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Types
  ( BoundingBox (..)
  , Material (..)
  , Variation (..)
  , Drop (..)
  ) where

import qualified Data.Text as T

data BoundingBox = BoundAsBlock | BoundAsEmpty deriving (Show,Eq)

data Material
  = Rock
  | Wood
  | Plant
  | Melon
  | Leaves
  | Dirt
  | Web
  | Wool
  deriving (Show,Eq)

data Variation = Variation
    { variantMetadata       :: Int
    , variantDisplayName    :: T.Text
    } deriving (Show,Eq)

data Drop = Drop
    { dropID        :: Int
    , dropMinCount  :: Maybe Int
    , dropMaxCount  :: Maybe Int
    , dropMetadata  :: Int
    } deriving (Show,Eq)
