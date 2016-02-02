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
    , BoundingBox (..)
    , Material (..)
    , Variation (..)
    , Drop (..)
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

data BoundingBox = BoundAsBlock | BoundAsEmpty deriving (Show,Eq)

data Material = Rock | Dirt | Wood | Plant | Wool deriving (Show,Eq)

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

data Block = Block
    { blockID       :: Int
    , displayName   :: T.Text
    , name          :: BC.ByteString
    , hardness      :: Maybe Rational
    , stackSize     :: Int
    , diggable      :: Bool
    , boundingBox   :: BoundingBox
    , material      :: Maybe Material
    , variations    :: Maybe Variation
    , harvestTools  :: [(Int,Bool)]
    , drops         :: [Int]
    } deriving (Show,Eq) 
