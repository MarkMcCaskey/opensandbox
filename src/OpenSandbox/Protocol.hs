{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol
  ( module OpenSandbox.Protocol.Compression
  , module OpenSandbox.Protocol.Encryption
  , module OpenSandbox.Protocol.Handle
  , module OpenSandbox.Protocol.Packet
  , module OpenSandbox.Protocol.Types
  , serializePacket
  , deserializePacket
  , fmtPacket
  , breakupPackets
  ) where

import OpenSandbox.Protocol.Compression
import OpenSandbox.Protocol.Encryption
import OpenSandbox.Protocol.Handle
import OpenSandbox.Protocol.Packet
import OpenSandbox.Protocol.Types

import Control.Monad.Catch
import qualified Data.ByteString as B
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Cereal
import Data.Serialize

fmtPacket :: (MonadIO m) => Conduit B.ByteString m B.ByteString
fmtPacket = awaitForever $ \payload ->
  yield ((runPut . putVarInt . B.length $ payload) `B.append` payload)

breakupPackets :: (MonadIO m, MonadThrow m) => Conduit B.ByteString m B.ByteString
breakupPackets = awaitForever $ \stream -> mapM_ yield (breakup stream)
  where
    breakup :: B.ByteString -> [B.ByteString]
    breakup "" = []
    breakup bs =
      case runGetPartial getVarInt bs of
        Fail _ _ -> error "Error: Failed to breakup packets!"
        Partial _ -> error "Error: Partial breakup?"
        Done ln leftover -> (B.take ln leftover):breakup (B.drop ln leftover)

serializePacket :: (Serialize a, MonadIO m) => Conduit a m B.ByteString
serializePacket = conduitPut put

deserializePacket :: (Serialize a, MonadThrow m) => Conduit B.ByteString m a
deserializePacket = conduitGet2 get
