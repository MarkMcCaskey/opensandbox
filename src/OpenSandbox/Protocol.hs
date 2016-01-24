{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol
  ( module P
  , ClientBoundPacket (..)
  , ServerBoundPacket (..)
  ) where

import Data.Serialize
import GHC.Generics

import OpenSandbox.Protocol.Login   as P
import OpenSandbox.Protocol.Play    as P
import OpenSandbox.Protocol.Status  as P

data ClientBoundPacket = CBS ClientBoundStatus | CBL ClientBoundLogin | CBP ClientBoundPlay
  deriving (Show,Eq,Generic)

instance Serialize ClientBoundPacket

data ServerBoundPacket = SBS ServerBoundStatus | SBL ServerBoundLogin | SBP ServerBoundPlay
  deriving (Show,Eq,Generic)

instance Serialize ServerBoundPacket
