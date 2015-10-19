module OpenSandbox.Minecraft.Protocol.Handshaking
  ( Yggdrasil (..)
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
import Linear.V3
import OpenSandbox.Minecraft.Protocol.Types


data Yggdrasil
    = Handshake Word8 B.ByteString Word16 Word8
  --  | Ping Word64
  --  | LegacyPing Word8
    deriving (Show,Eq,Read)


{-
-- Packet ID: 0x00
data Handshake = Handshake
  { protocolVersion   :: !VarInt
  , address           :: !B.ByteString
  , port              :: !Word16
  , nextState         :: !VarInt
  } deriving (Show,Eq,Read)

-- Packet ID: 0xFE
data LegacyPing = LegacyPing
  { payload   :: !Word8
  } deriving (Show,Eq,Read)
-}


parsePort :: B.ByteString -> Word16
parsePort b = toEnum ((shiftL l 8) + r)
  where (l:r:[]) = fmap fromEnum (B.unpack b)


{-
ping :: P.Parser Yggdrasil
ping = do
  lengthByte <- P.anyWord8
  P.word8 1
  payload <- P.take (fromEnum lengthByte :: Int)
  return $ Ping payload -- error


legacyPing :: P.Parser Yggdrasil
legacyPing = do
  lengthByte <- P.anyWord8
  P.word8 1
  payload <- P.take (fromEnum lengthByte :: Int)
  return $ LegacyPing payload -- error
-}


instance Binary Yggdrasil where
  put (Handshake version address port state) = do
    put (fromIntegral $ 6 + B.length address :: Word8)
    put (0 :: Word8)
    put version
    put (fromIntegral $ B.length address :: Word8)
    putByteString address
    put port
    put state

  get = do
    len <- getWord8
    packetID <- getWord8
    case packetID of
      0 -> Handshake <$> getWord8 <*> (getWord8 >>= (\x -> getByteString (fromIntegral x))) <*> getWord16be <*> getWord8
      --1 -> Ping <$> (get :: Word64)
  --    254 -> LegacyPing <$> (get :: Word8)
      _ -> fail "Unrecognized packet!"
