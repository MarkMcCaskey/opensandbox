-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Compression
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Compression
  ( Compression (..)
  , handleWithCompression
  , handleWithoutCompression
  ) where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Serialize
import Data.Word

import OpenSandbox.Protocol.Types (putVarInt)

data Compression = Disabled | Everything | Threshold Word16
  deriving (Show,Eq)

handleWithCompression :: (Serialize a, Monad m) => a -> ConduitM a B.ByteString m ()
handleWithCompression bs = do
  let uncompressedBS = encodeLazy bs
  let dataLn = runPut . putVarInt . B.length . BL.toStrict $ uncompressedBS
  let compressedBS = BL.toStrict . Zlib.compress $ uncompressedBS
  let packetData = dataLn `B.append` compressedBS
  let packetLn = runPut . putVarInt . B.length $ packetData
  yield (packetLn `B.append` packetData)

handleWithoutCompression :: (Serialize a, Monad m) => a -> ConduitM a B.ByteString m ()
handleWithoutCompression packet = do
  let bs = encode packet
  let ln = runPut . putVarInt . B.length $ bs
  yield (ln `B.append` bs)
