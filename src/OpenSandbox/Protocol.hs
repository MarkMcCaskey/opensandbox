{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
  , Stat (..)
  , Player (..)
  , PlayerListAction (..)
  , PlayerProperty (..)
  , putByteStringField
  , putVarInt
  , getVarInt
  ) where

import            Prelude hiding (max)
import qualified  Data.Aeson as Aeson
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Char8 as BC
import qualified  Data.ByteString.Lazy as BL
import            Data.Int
import            Data.Maybe
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.Serialize
import            Data.UUID
import qualified  Data.Vector as V
import            Data.Word
import            GHC.Generics
import            OpenSandbox.Types


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
      0 ->  ServerBoundLoginStart
              <$> (getVarInt >>= getByteString)
      1 ->  ServerBoundEncryptionResponse
              <$> (getVarInt >>= getByteString)
              <*> (getVarInt >>= getByteString)
      _ -> fail "Unknown of packet ID"

{-
data ChunkSection = ChunkSection
  { bitsPerBlock  :: !Word8
  , palette       :: !(Maybe (V.Vector Int))
  , dataArray     :: !B.ByteString
  , blockLight    :: !B.ByteString
  , skyLight      :: !(Maybe B.ByteString)
  } deriving (Show,Eq)


instance Serialize ChunkSection
-}

data Stat = Stat T.Text Int deriving (Show,Eq)


instance Serialize Stat where
  put (Stat statName statVal) = do
    putByteStringField . encodeUtf8 $ statName
    putVarInt statVal

  get = Stat <$> fmap decodeUtf8 (getVarInt >>= getByteString) <*> getVarInt


instance (Serialize a) => Serialize (V.Vector a) where
  put v = do
    putVarInt . V.length $ v
    (mapM_ put v)

  get = undefined


data Player = Player
  { playerUUID        :: UUID
  , playerListAction  :: PlayerListAction
  } deriving (Show,Eq)


instance Serialize Player where
  put (Player u pla) = do
    putByteString . BL.toStrict . toByteString $ u
    put pla

  get = undefined


data PlayerListAction
  = PlayerListAdd T.Text (V.Vector PlayerProperty) GameMode Int Bool (Maybe T.Text)
  | PlayerListUpdateGameMode GameMode
  | PlayerListUpdateLatency Int
  | PlayerListUpdateDisplayName Bool (Maybe T.Text)
  | PlayerListRemovePlayer
  deriving (Show,Eq)


instance Serialize PlayerListAction where
  put (PlayerListAdd name properties gameMode ping hasDisplayName displayName) = do
    putByteStringField . encodeUtf8 $ name
    put properties
    putVarInt . fromEnum $ gameMode
    putVarInt ping
    put hasDisplayName
    if displayName /= Nothing
      then do
        let displayPayload = encodeUtf8 . fromJust $ displayName
        putVarInt . B.length $ displayPayload
        putByteString displayPayload
      else
        return ()
  put (PlayerListUpdateGameMode gameMode) = do
    putVarInt . fromEnum $ gameMode
  put (PlayerListUpdateLatency ping) = do
    putVarInt ping
  put (PlayerListUpdateDisplayName hasDisplayName displayName) = do
    put hasDisplayName
    if displayName /= Nothing
      then do
        let displayPayload = encodeUtf8 . fromJust $ displayName
        putVarInt . B.length $ displayPayload
        putByteString displayPayload
      else
        return ()
  put PlayerListRemovePlayer = return ()
  get = undefined


data PlayerProperty = PlayerProperty
  { playerName    :: !T.Text
  , playerValue   :: !T.Text
  , isSigned      :: !Bool
  , playerSig     :: !(Maybe T.Text)
  } deriving (Show,Eq)


instance Serialize PlayerProperty where
  put (PlayerProperty pn pv is ps) = do
    let pnPayload = encodeUtf8 pn
    putVarInt . B.length $ pnPayload
    putByteString pnPayload
    let pvPayload = encodeUtf8 pv
    putVarInt . B.length $ pvPayload
    putByteString pvPayload
    put is
    if ps /= Nothing
      then do
        let sigPayload = encodeUtf8 . fromJust $ ps
        putVarInt . B.length $ sigPayload
        putByteString sigPayload
      else do
        return ()

  get = undefined


data ClientBoundPlay
  = ClientBoundStatistics (V.Vector Stat)
  | ClientBoundDifficulty Difficulty
  | ClientBoundCustomPayload B.ByteString B.ByteString
  | ClientBoundEntityStatus Int Word8
  | ClientBoundKeepAlive Int
  -- | ClientBoundChunkData Int32 Int32 Bool Int Int (V.Vector ChunkSection) (Maybe B.ByteString)
  | ClientBoundLogin Int32 GameMode Dimension Difficulty Word8 WorldType Bool
  | ClientBoundPlayerAbilities Bool Float Float
  | ClientBoundPlayerListItem Int (V.Vector Player)
  | ClientBoundHeldItemSlot Bool
  | ClientBoundUpdateTime Int64 Int64
  deriving (Show,Eq)


instance Serialize ClientBoundPlay where
  put (ClientBoundStatistics stats) = do
    let statPayload = encode stats
    putVarInt $ 1 + (B.length . runPut $ putByteStringField statPayload)
    putWord8 0x07
    putByteStringField statPayload
  put (ClientBoundDifficulty d) = do
    putVarInt 2
    putWord8 0x0d
    putWord8 . toEnum . fromEnum $ d
  put (ClientBoundCustomPayload channel dat)= do
    putVarInt $ 1 + 1 + B.length channel + 1 + B.length dat
    putWord8 0x18
    putByteStringField channel
    putByteStringField dat
  put (ClientBoundEntityStatus entityID entityStatus) = do
    putVarInt 6
    putWord8 0x1b
    putWord32be . toEnum $ entityID
    putWord8 entityStatus
  put (ClientBoundKeepAlive payload) = do
    putVarInt $ 1 + 4
    putWord8 0x1F
    putVarInt payload
  --put (ClientBoundChunkData chunkX chunkY groundUpContinuous bitmask chunkSize chunkData biomes) = undefined
  put (ClientBoundLogin entityId gameMode dimension difficulty maxPlayers levelType reducedDebugInfo) = do
    putVarInt (1 + 4 + 1 + 1 + 1 + 1 + 1 + (B.length . BC.pack . show $ levelType) + 1)
    putWord8 0x23
    putWord32be . toEnum . fromEnum $ entityId
    putWord8 . toEnum . fromEnum $ gameMode
    putWord8 . toEnum . fromEnum $ dimension
    putWord8 . toEnum . fromEnum $ difficulty
    putWord8 . toEnum . fromEnum $ maxPlayers
    putVarInt . B.length . BC.pack . show $ levelType
    putByteString . BC.pack . show $ levelType
    put reducedDebugInfo
  put (ClientBoundPlayerAbilities flags flyingSpeed walkingSpeed) = do
    putVarInt 10
    putWord8 0x2b
    put flags
    putWord32be . toEnum . fromEnum $ flyingSpeed
    putWord32be . toEnum . fromEnum $ walkingSpeed
  put (ClientBoundPlayerListItem action players) = do
    putWord8 0x2d
  put (ClientBoundHeldItemSlot slot) = do
    putVarInt 2
    putWord8 0x37
    put slot
  put (ClientBoundUpdateTime age time) = do
    putVarInt $ 1 + 8
    putWord8 0x43
    putWord64be . toEnum . fromEnum $ age
    putWord64be . toEnum . fromEnum $ time

  get = do
    _ <- getVarInt
    packetID <- getWord8
    case packetID of
      0x07 -> ClientBoundStatistics
                <$> get
      0x0d -> ClientBoundDifficulty
                <$> (fmap (toEnum . fromEnum) getWord8)
      0x18 -> ClientBoundCustomPayload
                <$> (getVarInt >>= getByteString)
                <*> (getVarInt >>= getByteString)
      0x1b -> ClientBoundEntityStatus
                <$> (fmap fromEnum getWord32be)
                <*> getWord8
      0x1F -> ClientBoundKeepAlive
                <$> getVarInt
      --0x20 -> ClientBoundChunkData
      0x2b -> ClientBoundPlayerAbilities
                <$> get
                <*> get
                <*> get
      0x2d -> ClientBoundPlayerListItem
                <$> get
                <*> get
      0x37 -> ClientBoundHeldItemSlot
                <$> get
      0x43 -> ClientBoundUpdateTime
                <$> get
                <*> get
      _    -> undefined


data ServerBoundPlay
  = ServerBoundKeepAlive Int
  deriving (Show,Eq)

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

putByteStringField :: Serialize a => a -> PutM ()
putByteStringField x = do
  let payload = runPut (put x)
  let len = B.length payload
  if len /= 0
    then do putVarInt len
            putByteString payload
    else do putVarInt len
{-# INLINE putByteStringField #-}


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
