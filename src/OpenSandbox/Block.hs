{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Block
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Block
  ( Block (..)
  ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

import OpenSandbox.Types


data Block = Block
    { blockID             :: Int
    , blockDisplayName    :: T.Text
    , blockName           :: BC.ByteString
    , blockHardness       :: Maybe Rational
    , blockStackSize      :: Int
    , blockDiggable       :: Bool
    , blockBoundingBox    :: BoundingBox
    , blockMaterial       :: Maybe Material
    , blockVariations     :: Maybe [Variation]
    , blockHarvestTools   :: [(Int,Bool)]
    , blockDrops          :: [Int]
    } deriving (Show,Eq)
