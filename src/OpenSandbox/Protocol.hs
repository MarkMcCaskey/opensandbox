{-# LANGUAGE DeriveGeneric #-}
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
  ( ClientBoundStatus (..)
  , ServerBoundStatus (..)
  , ClientBoundLogin (..)
  , ServerBoundLogin (..)
  , ClientBoundPlay (..)
  , ServerBoundPlay (..)
  , putByteStringField
  ) where

import            Prelude hiding (max)
import qualified  Data.Aeson as Aeson
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Char8 as BC
import qualified  Data.ByteString.Lazy as BL
import            Data.Int
import qualified  Data.Text as T
import            Data.Serialize
import            Data.Word
import            GHC.Generics
import            OpenSandbox.Types


-- Adapted from the protocol-buffers library, but only for Serialize and Ints

putVarInt :: Int -> Put
putVarInt i | i < 0x80 = putWord8 (fromIntegral i)
            | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> putVarInt (i `shiftR` 7)
{-# INLINE putVarInt #-}

getVarInt :: Get Int
getVarInt = do
    w <- getWord8
    if testBit w 7 then go 7 (fromIntegral (w .&. 0x7F))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- getWord8
      if testBit w' 7 then go (n+7) (val .|. ((fromIntegral (w' .&. 0x7F)) `shiftL` n))
        else return (val .|. ((fromIntegral w') `shiftL` n))
{-# INLINE getVarInt #-}

-------------------------------------------------------------------------------


data ClientBoundStatus
  = ClientBoundResponse T.Text Word8 Word8 Word8 T.Text
  | ClientBoundPong Int64
  deriving (Show,Eq)


instance Serialize ClientBoundStatus where
  put (ClientBoundResponse mcversion versionID currentPlayers maxPlayers motd) = do
    let jsonPayload =
          BL.toStrict . Aeson.encode $
            StatusPayload (Version mcversion versionID)
                          (Players maxPlayers currentPlayers)
                          (Description motd)
    putVarInt (2 + B.length jsonPayload :: Int)
    put (0x00 :: Word8)
    putVarInt $ B.length jsonPayload
    putByteString jsonPayload
  put (ClientBoundPong payload) = do
    putVarInt (1 + 8 :: Int)
    put (0x01 :: Word8)
    put payload

  get = do
    _ <- getVarInt :: Get Int
    packetID <- getWord8
    case packetID of
      0x00 -> do  jsonBinary <- (getVarInt >>= getByteString)
                  let possibleJson = Aeson.eitherDecodeStrict jsonBinary
                  case possibleJson of
                    Left err -> fail err
                    Right json -> return $ ClientBoundResponse
                                        (name . version $ json)
                                        (protocol . version $ json)
                                        (online . players $ json)
                                        (max . players $ json)
                                        (text . description $ json)

      0x01 -> ClientBoundPong <$> (get :: Get Int64)
      _ -> fail "Unrecognized packet!"


data ServerBoundStatus
  = ServerBoundHandshake Word B.ByteString Word16 Word
  | ServerBoundPingStart
  | ServerBoundPing Int64
  deriving (Show,Eq)

instance Serialize ServerBoundStatus where
  put (ServerBoundHandshake protoVersion srvAddress srvPort nextState) = do
    putVarInt (6 + B.length srvAddress :: Int)
    put (0 :: Word8)
    putVarInt . fromEnum $ protoVersion
    putVarInt $ B.length srvAddress
    putByteString srvAddress
    putWord16be srvPort
    putVarInt . fromEnum $ nextState
  put ServerBoundPingStart = do
    put (1 :: Word8)
    put (0 :: Word8)
  put (ServerBoundPing payload) = do
    putVarInt (2 + 8 :: Int)
    put (1 :: Word8)
    put payload

  get = do
    len <- getVarInt
    packetID <- getWord8
    case packetID of
      0 -> case len of
            1 ->  return ServerBoundPingStart
            _ ->  ServerBoundHandshake
                  <$> (fmap toEnum getVarInt)
                  <*> (getVarInt >>= getByteString)
                  <*> getWord16be
                  <*> (fmap toEnum getVarInt)
      1 -> ServerBoundPing <$> (get :: Get Int64)
      _ -> fail "Unrecognized packet!"


data ClientBoundLogin
  = ClientBoundDisconnect B.ByteString
  | ClientBoundEncryptionRequest B.ByteString B.ByteString B.ByteString
  | ClientBoundLoginSuccess B.ByteString B.ByteString
  | ClientBoundSetCompression Word16
  deriving (Show,Eq)


instance Serialize ClientBoundLogin where
  put (ClientBoundDisconnect reason) = do
    putVarInt $ 3 + B.length reason
    put (0 :: Word8)
    putVarInt $ B.length reason
    putByteString reason
  put (ClientBoundEncryptionRequest srvID pubKey privKey) = do
    putVarInt $ 5 + B.length srvID + B.length pubKey + B.length privKey
    put (1 :: Word8)
    putVarInt $ B.length srvID
    putByteString srvID
    putVarInt $ B.length pubKey
    putByteString pubKey
    putVarInt $ B.length privKey
    putByteString privKey
  put (ClientBoundLoginSuccess uuid username) = do
    putVarInt $ 3 + B.length uuid + B.length username
    put (2 :: Word8)
    putVarInt $ B.length uuid
    putByteString uuid
    putVarInt $ B.length username
    putByteString username
  put (ClientBoundSetCompression compressionFlag) = do
    putVarInt $ 4
    put (3 :: Word8)
    putWord16be compressionFlag

  get = do
    _ <- getVarInt
    packetID <- getWord8
    case packetID of
      0 -> ClientBoundDisconnect
            <$> (getVarInt >>= getByteString)
      1 -> ClientBoundEncryptionRequest
            <$> (getVarInt >>= getByteString)
            <*> (getVarInt >>= getByteString)
            <*> (getVarInt >>= getByteString)
      2 -> ClientBoundLoginSuccess
            <$> (getVarInt >>= getByteString)
            <*> (getVarInt >>= getByteString)
      3 -> ClientBoundSetCompression <$> getWord16be
      _ -> fail "Unknown packet ID"


data ServerBoundLogin
  = ServerBoundLoginStart B.ByteString
  | ServerBoundEncryptionResponse B.ByteString B.ByteString
  deriving (Show,Eq)


instance Serialize ServerBoundLogin where
  put (ServerBoundLoginStart payload) = do
    putVarInt $ 3 + B.length payload
    put (0 :: Word8)
    putVarInt $ B.length payload
    putByteString payload
  put (ServerBoundEncryptionResponse cert token) = do
    putVarInt $ 2 + B.length cert + B.length token
    put (1 :: Word8)
    putVarInt $ B.length cert
    putByteString cert
    putVarInt $ B.length token
    putByteString token

  get = do
    _ <- getVarInt
    packetID <- getWord8
    case packetID of
      0 -> ServerBoundLoginStart <$> (getVarInt >>= getByteString)
      1 -> ServerBoundEncryptionResponse <$> (getVarInt >>= getByteString)
                                          <*> (getVarInt >>= getByteString)
      _ -> fail "Unknown of packet ID"


data ClientBoundPlay
  = ClientBoundKeepAlive Int
  | ClientBoundLogin Int32 GameMode Dimension Difficulty Word8 WorldType Bool
  | ClientBoundUpdateTime Int64 Int64
  | ClientBoundHeldItemSlot Bool
  -- | ClientBoundStatistics Word8 B.ByteString
  | ClientBoundPlayerAbilities Bool Float Float
  | ClientBoundCustomPayload B.ByteString B.ByteString
  | ClientBoundDifficulty Difficulty
  deriving (Show,Eq)


instance Serialize ClientBoundPlay where
  put (ClientBoundLogin entityId gameMode dimension difficulty maxPlayers levelType reducedDebugInfo) = do
    put (fromIntegral $ 1 + 4 + 1 + 1 + 1 + 1 + 1 + (B.length . BC.pack . show $ levelType) + 1 :: Word8)
    put (0x23 :: Word8)
    putWord32be . toEnum . fromEnum $ entityId
    putWord8 . toEnum . fromEnum $ gameMode
    putWord8 . toEnum . fromEnum $ dimension
    putWord8 . toEnum . fromEnum $ difficulty
    putWord8 . toEnum . fromEnum $ maxPlayers
    putVarInt . B.length . BC.pack . show $ levelType
    putByteString . BC.pack . show $ levelType
    put reducedDebugInfo
  put (ClientBoundUpdateTime age time) = do
    putWord8 . toEnum $ 1 + 8
    put (0x43 :: Word8)
    putWord64be . toEnum . fromEnum $ age
    putWord64be . toEnum . fromEnum $ time
  put (ClientBoundHeldItemSlot slot) = do
    put (2 :: Word8)
    put (0x37 :: Word8)
    put slot
  put (ClientBoundPlayerAbilities flags flyingSpeed walkingSpeed) = do
    put (10 :: Word8)
    put (0x2b :: Word8)
    put flags
    putWord32be . toEnum . fromEnum $ flyingSpeed
    putWord32be . toEnum . fromEnum $ walkingSpeed
  put (ClientBoundCustomPayload channel dat)= do
    put (fromIntegral $ 1 + 1 + B.length channel + 1 + B.length dat :: Word8)
    put (0x18 :: Word8)
    putByteStringField channel
    putByteStringField dat
  put (ClientBoundDifficulty d) = do
    put (2 :: Word8)
    put (0x0d :: Word8)
    putWord8 . toEnum . fromEnum $ d
  put (ClientBoundKeepAlive payload) = do
    putVarInt $ 1 + 4
    putWord8 0x1F
    putVarInt payload

  get = do
    _ <- getWord8
    packetID <- getWord8
    case packetID of
      --0x07 -> ClientBoundStatistics
      0x0d -> ClientBoundDifficulty <$> (fmap (toEnum . fromEnum) getWord8)
      0x18 -> ClientBoundCustomPayload
                <$> (getVarInt >>= getByteString)
                <*> (getVarInt >>= getByteString)
      0x2b -> ClientBoundPlayerAbilities
                <$> fmap (toEnum . fromEnum) getWord8
                <*> fmap (toEnum . fromEnum) getWord32be
                <*> fmap (toEnum . fromEnum) getWord32be
      _    -> undefined


data ServerBoundPlay
  = ServerBoundKeepAlive Int
  deriving (Show,Eq)


putByteStringField :: Serialize a => a -> PutM ()
putByteStringField x = do
  let payload = runPut (put x)
  let len = B.length payload
  if len /= 0
    then do putVarInt len
            putByteString payload
    else do putVarInt len


instance Serialize ServerBoundPlay where
  put (ServerBoundKeepAlive keepAliveId) = do
    putWord8 . toEnum $ 1 + 4
    putWord8 0x0a
    putVarInt keepAliveId

  get = do
    _ <- getWord8
    packetID <- getWord8
    case packetID of
      0x0a -> ServerBoundKeepAlive <$> getVarInt
      _    -> undefined


data StatusPayload = StatusPayload
  { version       :: Version
  , players       :: Players
  , description   :: Description
  } deriving (Generic,Show,Eq,Read)


instance Aeson.ToJSON StatusPayload
instance Aeson.FromJSON StatusPayload


data Version = Version
  { name      :: T.Text
  , protocol  :: Word8
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Version
instance Aeson.FromJSON Version


data Players = Players
  { max     :: Word8
  , online  :: Word8
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Players
instance Aeson.FromJSON Players


data Description = Description
  { text    :: T.Text
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Description
instance Aeson.FromJSON Description
