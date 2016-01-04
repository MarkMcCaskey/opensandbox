-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.Protocol.Login
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-- Bindings to the login section of the Minecraft protocol.
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Protocol.Login
  ( ClientBoundLogin (..)
  , ServerBoundLogin (..)
  , encryptionRequestPacket
  , loginSuccess
  ) where


import            Data.Serialize
import            Data.Serialize.Get
import            Data.Serialize.Put
--import            Data.Binary
--import            Data.Binary.Get
--import            Data.Binary.Put
import qualified  Data.ByteString as B
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID
import            Data.Word

import            OpenSandbox.Minecraft.User


-- | A data type that could represent any client bound packet associated with
-- the login section of the minecraft protocol.
data ClientBoundLogin
  = ClientBoundDisconnect B.ByteString
  | ClientBoundEncryptionRequest B.ByteString B.ByteString B.ByteString
  | ClientBoundLoginSuccess B.ByteString B.ByteString
  | ClientBoundSetCompression Word16
  deriving (Show,Eq)


-- | A data type that could represent any server bound packet associated with
-- the login section of the minecraft protocol.
data ServerBoundLogin
  = ServerBoundLoginStart B.ByteString
  | ServerBoundEncryptionResponse B.ByteString B.ByteString
  deriving (Show,Eq)


instance Serialize ClientBoundLogin where
  put (ClientBoundDisconnect reason) = do
    put (fromIntegral $ 3 + B.length reason :: Word8)
    put (0 :: Word8)
    put (fromIntegral $ B.length reason :: Word8)
    putByteString reason
  put (ClientBoundEncryptionRequest srvID pubKey privKey) = do
    put (fromIntegral $ 5 + B.length srvID + B.length pubKey + B.length privKey :: Word8)
    put (1 :: Word8)
    put (1 :: Word8)
    put (0 :: Word8)
    put (fromIntegral $ B.length pubKey :: Word8)
    put (1 :: Word8)
    putByteString pubKey
    put (fromIntegral $ B.length privKey :: Word8)
    putByteString privKey
  put (ClientBoundLoginSuccess uuid username) = do
    put (fromIntegral $ 4 + B.length uuid + B.length username :: Word8)
    put (2 :: Word8)
    put (fromIntegral $ B.length uuid :: Word8)
    putByteString uuid
    put (fromIntegral $ B.length username :: Word8)
    putByteString username

  get = do
    len <- getWord8
    packetID <- getWord8
    case packetID of
      0 -> ClientBoundDisconnect
            <$> (getWord8 >>= (getByteString . fromIntegral))
      1 -> ClientBoundEncryptionRequest
            <$> (getWord8 >>= (getByteString . fromIntegral))
            <*> (fmap (B.drop 1) (getWord8 >>= (getByteString . fromIntegral)))
            <*> (getWord8 >>= (getByteString . fromIntegral))
      2 -> ClientBoundLoginSuccess
            <$> (getWord8 >>= (getByteString . fromIntegral))
            <*> (getWord8 >>= (getByteString . fromIntegral))
      3 -> ClientBoundSetCompression <$> getWord16be


instance Serialize ServerBoundLogin where
  put (ServerBoundLoginStart payload) = do
    put (fromIntegral $ 3 + B.length payload :: Word8)
    put (0 :: Word8)
    put (fromIntegral $ B.length payload :: Word8)
    putByteString payload
  put (ServerBoundEncryptionResponse cert token) = do
    put (fromIntegral $ 2 + B.length cert + B.length token :: Word8)
    put (1 :: Word8)
    put (fromIntegral $ B.length cert :: Word8)
    putByteString cert
    put (fromIntegral $ B.length token :: Word8)
    putByteString token

  get = do
    len <- getWord8
    packetID <- getWord8
    case packetID of
      0 -> ServerBoundLoginStart <$> (getWord8 >>= (getByteString . fromIntegral))
      1 -> ServerBoundEncryptionResponse <$> (getWord8 >>= (getByteString . fromIntegral))
                                          <*> (getWord8 >>= (getByteString . fromIntegral))


encryptionRequestPacket :: B.ByteString -> B.ByteString -> B.ByteString
encryptionRequestPacket c v = packetLength `B.append` packetID `B.append` payload
  where packetLength = B.pack [(fromIntegral $ B.length payload :: Word8)]
        packetID = B.singleton 1
        payload = serverIDField `B.append` publicKeyField c `B.append` verifyTokenField v


loginSuccess :: User -> B.ByteString
loginSuccess u = packetLength `B.append` packetID `B.append` payload u
    where packetLength = B.pack [(fromIntegral $ B.length (payload u) + B.length packetID :: Word8)]
          packetID = B.singleton 2
          payload u = fieldLength (packUUID u)
                      `B.append` (packUUID u)
                      `B.append` fieldLength (packUser u)
                      `B.append` (packUser u)
          packUUID u = toASCIIBytes $ userUUID u
          packUser u = encodeUtf8 $ userName u

fieldLength :: B.ByteString -> B.ByteString
fieldLength payload = B.pack [(fromIntegral $ B.length payload :: Word8)]


serverIDField :: B.ByteString
serverIDField = packetLength `B.append` payload
  where packetLength = B.pack [(fromIntegral $ B.length payload :: Word8)]
        payload = B.singleton 0


publicKeyField :: B.ByteString -> B.ByteString
publicKeyField payload = packetLength `B.append` mystery `B.append` payload
  where packetLength = B.pack [(fromIntegral $ B.length payload :: Word8)]
        mystery = B.singleton 1


verifyTokenField :: B.ByteString -> B.ByteString
verifyTokenField payload = packetLength `B.append` payload
  where packetLength = B.pack [(fromIntegral $ B.length payload :: Word8)]


