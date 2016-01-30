{-# LANGUAGE GADTs #-}
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
  , ProtocolState (..)
  ) where

import Data.Serialize

import OpenSandbox.Protocol.Login   as P
import OpenSandbox.Protocol.Play    as P
import OpenSandbox.Protocol.Status  as P

data ProtocolState = Handshake | Status | Login | Play deriving (Show,Eq)
