{-# LANGUAGE DeriveGeneric #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.Protocol.Status
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Protocol.Status
    ( Response (..)
    , Version (..)
    , Players (..)
    , Description (..)
    ) where


import Data.Aeson
import qualified Data.Text as T
import GHC.Generics


data Response = Response
  { version       :: Version
  , players       :: Players
  , description   :: Description
  } deriving (Generic,Show)


instance ToJSON Response
instance FromJSON Response


data Version = Version
  { name      :: T.Text
  , protocol  :: Int
  } deriving (Generic,Show)


instance ToJSON Version
instance FromJSON Version


data Players = Players
  { max     :: Int
  , online  :: Int
  } deriving (Generic,Show)


instance ToJSON Players
instance FromJSON Players


data Description = Description
  { text    :: T.Text
  } deriving (Generic,Show)


instance ToJSON Description
instance FromJSON Description
