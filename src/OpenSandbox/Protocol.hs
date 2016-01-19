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
  , ClientBoundPacket
  , ServerBoundPacket
  ) where

import OpenSandbox.Protocol.Login   as P
import OpenSandbox.Protocol.Play    as P
import OpenSandbox.Protocol.Status  as P

data ClientBoundPacket =
  ClientBoundLogin | ClientBoundPlay | ClientBoundStatus deriving (Show,Eq)

data ServerBoundPacket =
  ServerBoundLogin | ServerBoundPlay | ServerBoundStatus deriving (Show,Eq)
