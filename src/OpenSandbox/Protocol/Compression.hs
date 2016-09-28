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
  , compressPacket
  , decompressPacket
  ) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Serialize
import Data.Word

import OpenSandbox.Protocol.Types (putVarInt,getVarInt,Session(..))

data Compression = Disabled | Everything | Threshold Word16
  deriving (Show,Eq)

compressPacket :: Conduit B.ByteString (StateT Session IO) B.ByteString
compressPacket = awaitForever $ \packet -> do
  s <- lift State.get
  if (sessionCompressionIsActive s) && (sessionCompressionIsEnabled s)
    then do
      let dataLn = runPut . putVarInt . B.length $ packet
      let compressedBS = BL.toStrict . Zlib.compress . BL.fromStrict $ packet
      let packetData = dataLn `B.append` compressedBS
      yield packetData
    else yield packet

decompressPacket :: Conduit B.ByteString (StateT Session IO) B.ByteString
decompressPacket = awaitForever $ \compressedBS -> do
  s <- lift State.get
  if (sessionCompressionIsActive s) && (sessionCompressionIsEnabled s)
    then case runGet getCompressed compressedBS of
           Left err -> error err
           Right decompressedBS -> yield decompressedBS
    else yield compressedBS
 where
  getCompressed = do
    _ <- getVarInt
    r <- remaining
    compressedBS <- getLazyByteString (toEnum r)
    return $ BL.toStrict $ Zlib.decompressWith Zlib.defaultDecompressParams compressedBS
