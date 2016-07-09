{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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
  ( ProtocolState (..)
  , BoundingBox (..)
  , Material (..)
  , Variation (..)
  , Drop (..)
  , Dimension (..)
  , Difficulty (..)
  , GameMode (..)
  , WorldType (..)
  , Compression (..)
  , Encryption (..)
  ) where

import            Crypto.PubKey.RSA
import            Data.Aeson
import qualified  Data.ByteString as B
import qualified  Data.Text as T
import            GHC.Generics

data ProtocolState = Handshake | Status | Login | Play deriving (Show,Eq)


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


data Dimension = Overworld | Nether | End
  deriving (Show,Eq,Generic)


instance Enum Dimension where
    fromEnum Overworld = 0
    fromEnum Nether = -1
    fromEnum End = 1
    toEnum 0 = Overworld
    toEnum (-1) = Nether
    toEnum 1 = End
    toEnum _ = undefined

instance FromJSON Dimension
instance ToJSON Dimension

data Difficulty = Peaceful | Easy | Normal | Hard
  deriving (Show,Enum,Eq,Generic)


instance FromJSON Difficulty
instance ToJSON Difficulty


data GameMode = Survival | Creative | Adventure | Spectator
  deriving (Show,Enum,Eq,Generic)


instance FromJSON GameMode
instance ToJSON GameMode


data WorldType = Default | Flat | LargeBiomes | Amplified
  deriving (Eq,Generic)

instance FromJSON WorldType
instance ToJSON WorldType

instance Show WorldType where
    show Default = "default"
    show Flat = "flat"
    show LargeBiomes = "largeBiomes"
    show Amplified = "amplified"


data Compression = Everything | Int
  deriving (Show,Eq)


data Encryption = Encryption
  { getCert         :: B.ByteString
  , getPubKey       :: PublicKey
  , getPrivKey      :: PrivateKey
  , getVerifyToken  :: B.ByteString
  } deriving (Show,Eq)
