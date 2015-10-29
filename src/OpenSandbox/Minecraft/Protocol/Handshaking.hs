-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.Protocol.Handshaking
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Protocol.Handshaking
    ( ServerBoundStatus (..)
    ) where


import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Int
import Data.Word
import GHC.Generics


data ServerBoundStatus
    = Handshake Word8 B.ByteString Word16 Word8
    | Request
    | Ping Word64
    deriving (Show,Eq,Read)


instance Binary ServerBoundStatus where
  put (Handshake version address port state) = do
    put (fromIntegral $ 6 + B.length address :: Word8)
    put (0 :: Word8)
    put version
    put (fromIntegral $ B.length address :: Word8)
    putByteString address
    put port
    put state
  put (Ping payload) = do
    put (fromIntegral $ 2 + 8 :: Word8)
    put (1 :: Word8)
    putWord64be payload

  get = do
    len <- getWord8
    packetID <- getWord8
    case packetID of
      0 -> Handshake <$> getWord8 <*> (getWord8 >>= (\x -> getByteString (fromIntegral x))) <*> getWord16be <*> getWord8
      1 -> Ping <$> (get :: Get Word64)
      _ -> fail "Unrecognized packet!"


parsePort :: B.ByteString -> Word16
parsePort b = toEnum ((shiftL l 8) + r)
  where (l:r:[]) = fmap fromEnum (B.unpack b)

