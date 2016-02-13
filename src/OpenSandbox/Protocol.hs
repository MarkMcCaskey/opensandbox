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
  ) where

import            Prelude hiding (max)
import qualified  Data.Aeson as Aeson
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import            Data.Int
import qualified  Data.Text as T
import            Data.Serialize
import            Data.Word
import            GHC.Generics
import            Numeric.Natural
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
  = ClientBoundResponse T.Text Int Int Int T.Text
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
  = ServerBoundHandshake Word8 B.ByteString Word16 Word8
  | ServerBoundPingStart
  | ServerBoundPing Int64
  deriving (Show,Eq)


instance Serialize ServerBoundStatus where
  put (ServerBoundHandshake v a p s) = do
    putVarInt (6 + B.length a :: Int)
    put (0 :: Word8)
    put v
    putVarInt $ B.length a
    putByteString a
    put p
    put s
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
            1 -> return ServerBoundPingStart
            _ -> ServerBoundHandshake <$> getWord8 <*> (getVarInt >>= getByteString) <*> getWord16be <*> getWord8
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
  | ClientBoundLogin Int32 GameMode Dimension Difficulty Natural T.Text Bool
  -- | ClientBoundChat MC_String MC_Byte
  -- | ClientBoundUpdateTime MC_Int MC_Int -- MC_Long MC_Long
  -- | ClientBoundEntityEquipment MC_VarInt MC_VarInt MC_Slot
  -- | ClientBoundSpawnPosition MC_Position
  -- | ClientBoundUpdateHealth MC_Float MC_VarInt MC_Float
  -- | ClientBoundRespawn MC_Int MC_UByte MC_UByte MC_String
  -- | ClientBoundPosition MC_Double MC_Double MC_Double MC_Float MC_Float MC_Byte
  -- | ClientBoundHeldItemSlot MC_Byte
  -- | ClientBoundBed MC_VarInt MC_Position
  -- | ClientBoundAnimation MC_VarInt MC_UByte
  -- | ClientBoundNamedEntitySpawn MC_VarInt MC_UUID MC_Int MC_Int MC_Int MC_Byte MC_Byte MC_Metadata
  -- | ClientBoundCollect MC_VarInt MC_VarInt
  -- | ClientBoundSpawnEntity MC_VarInt MC_UUID MC_Byte MC_Int MC_Int MC_Int MC_Byte MC_Byte MC_Int MC_Short MC_Short MC_Short
  -- | ClientBoundSpawnEntityLiving MC_VarInt MC_UUID MC_UByte MC_Int MC_Int MC_Int MC_Byte MC_Byte MC_Byte MC_Short MC_Short MC_Short MC_Metadata
  -- | ClientBoundSpawnEntityPainting MC_VarInt MC_String MC_Position MC_UByte
  -- | ClientBoundSpawnEntityExperienceOrb MC_VarInt MC_Int MC_Int MC_Int MC_Short
  -- | ClientBoundEntityVelocity MC_VarInt MC_Short MC_Short MC_Short
  -- | ClientBoundEntityDestroy MC_Array
  -- | ClientBoundEntity MC_VarInt
  -- | ClientBoundRelEntityMove MC_VarInt MC_Byte MC_Byte MC_Byte MC_Bool
  -- | ClientBoundEntityLook MC_VarInt MC_Byte MC_Byte MC_Bool
  -- | ClientBoundEntityMoveLook MC_VarInt MC_Byte MC_Byte MC_Byte MC_Byte MC_Byte MC_Bool
  -- | ClientBoundEntityTeleport MC_VarInt MC_Int MC_Int MC_Int MC_Byte MC_Byte MC_Bool
  -- | ClientBoundEntityHeadRotation MC_VarInt MC_Byte
  -- | ClientBoundEntityStatus MC_Int MC_Byte
  -- | ClientBoundAttachEntity MC_Int MC_Int MC_Bool
  -- | ClientBoundEntityMetadata MC_VarInt MC_Metadata
  -- | ClientBoundEntityEffect MC_VarInt MC_Byte MC_Byte MC_VarInt MC_Bool
  -- | ClientBoundRemoveEntityEffect MC_VarInt MC_Byte
  -- | ClientBoundExperience MC_Float MC_VarInt MC_VarInt
  -- | ClientBoundUpdateAttributes MC_VarInt MC_Array
  -- | ClientBoundMapChunk MC_Int MC_Int MC_Bool MC_VarInt MC_Array
  -- | ClientBoundMultiBlockChange MC_Int MC_Int MC_Array
  -- | ClientBoundBlockChange MC_Position MC_VarInt
  -- | ClientBoundBlockAction MC_Position MC_UByte MC_UByte MC_VarInt
  -- | ClientBoundBlockBreakAnimation MC_VarInt MC_Position MC_Byte
  -- | ClientBoundExplosion MC_Float MC_Float MC_Float MC_Float MC_Array MC_Float MC_Float MC_Float
  -- | ClientBoundWorldEvent MC_Int MC_Position MC_Int MC_Bool
  -- | ClientBoundNamedSoundEffect MC_String MC_Int MC_Int MC_Int MC_Float MC_UByte
  -- | ClientBoundWorldParticles MC_Int MC_Bool MC_Float MC_Float MC_Float MC_Float MC_Float MC_Float MC_Float MC_Int MC_Array
  -- | ClientBoundGameStateChange MC_UByte MC_Float
  -- | ClientBoundSpawnEntityWeather MC_VarInt MC_Byte MC_Int MC_Int MC_Int
  -- | ClientBoundOpenWindow MC_UByte MC_String MC_String MC_UByte MC_Array
  -- | ClientBoundCloseWindow MC_UByte
  -- | ClientBoundSetSlot MC_Byte MC_Short MC_Slot
  -- | ClientBoundWindowItems MC_UByte MC_Array
  -- | ClientBoundCraftProgressBar MC_UByte MC_Short MC_Short
  -- | ClientBoundTransaction MC_Byte MC_Short MC_Bool
  -- | ClientBoundUpdateSign MC_Position MC_String MC_String MC_String MC_String
  -- | ClientBoundMap MC_VarInt MC_Byte MC_Bool MC_Array MC_Byte MC_Array MC_Array MC_Array MC_Array
  -- | ClientBoundTileEntityData MC_Position MC_UByte MC_Optional
  -- | ClientBoundOpenSignEntity MC_Position
  -- | ClientBoundStatistics Word8 B.ByteString
  -- | ClientBoundPlayerInfo MC_VarInt MC_Array
  -- | ClientBoundAbilities MC_Byte MC_Float MC_Float
  -- | ClientBoundTabComplete MC_Array
  -- | ClientBoundScoreBoardObjective MC_String MC_Byte MC_Array MC_Array
  -- | ClientBoundScoreBoardScore MC_String MC_Byte MC_String MC_Array
  -- | ClientBoundScoreBoardDisplayObjective MC_Byte MC_String
  -- | ClientBoundScoreBoardTeam MC_String MC_Byte MC_Array MC_Array MC_Array MC_Array MC_Array MC_Array MC_Array MC_Array
  -- | ClientBoundCustomPayload MC_String MC_String
  -- | ClientBoundKickDisconnect MC_String
  -- | ClientBoundDifficulty MC_UByte
  -- | ClientBoundCombatEvent MC_VarInt MC_Array MC_Array MC_Array MC_Array
  -- | ClientBoundCamera MC_VarInt
  -- | ClientBoundWorldBorder MC_VarInt MC_Array MC_Array MC_Array MC_Array MC_Array MC_Array MC_Array MC_Array MC_Array
  -- | ClientBoundTitle MC_VarInt MC_Array MC_Array MC_Array MC_Array
  -- | ClientBoundPlaySetCompression MC_VarInt
  -- | ClientBoundPlayerlistHeader MC_String MC_String
  -- | ClientBoundResourcePackSend MC_String MC_String
  -- | ClientBoundBossBar MC_UUID MC_VarInt MC_Array MC_Array MC_Array MC_Array MC_Array
  -- | ClientBoundSetCooldown MC_VarInt MC_VarInt
  -- | ClientBoundUnloadChunk MC_Int MC_Int
  deriving (Show,Eq)


data ServerBoundPlay
  = ServerBoundKeepAlive Int
  -- | ServerBoundChat MC_String
  -- | ServerBoundUseEntity MC_VarInt MC_VarInt MC_Array MC_Array MC_Array MC_Array
  -- | ServerBoundFlying MC_Bool
  -- | ServerBoundPosition MC_Double MC_Double MC_Double MC_Bool
  -- | ServerBoundLook MC_Float MC_Float MC_Bool
  -- | ServerBoundPositionLook MC_Double MC_Double MC_Double MC_Float MC_Float MC_Bool
  -- | ServerBoundBlockDig MC_Byte MC_Position MC_Byte
  -- | ServerBoundBlockPlace MC_Position MC_VarInt MC_VarInt MC_Byte MC_Byte MC_Byte
  -- | ServerBoundHeldItemSlot MC_Short
  -- | ServerBoundArmAnimation MC_VarInt
  -- | ServerBoundEntityAction MC_VarInt MC_VarInt MC_VarInt
  -- | ServerBoundSteerVehicle MC_Float MC_Float MC_UByte
  -- | ServerBoundCloseWindow MC_UByte
  -- | ServerBoundWindowClick MC_UByte MC_Short MC_Byte MC_Short MC_Byte MC_Slot
  -- | ServerBoundTransaction MC_Byte MC_Short MC_Bool
  -- | ServerBoundSetCreativeSlot MC_Short MC_Slot
  -- | ServerBoundEnchantItem MC_Byte MC_Byte
  -- | ServerBoundUpdateSign MC_Position MC_String MC_String MC_String MC_String
  -- | ServerBoundAbilities MC_Byte MC_Float MC_Float
  -- | ServerBoundTabComplete MC_String [MC_Position]
  -- | ServerBoundSettings MC_String MC_Byte MC_VarInt MC_Bool MC_UByte MC_VarInt
  -- | ServerBoundClientCommand MC_VarInt
  -- | ServerBoundCustomPayload MC_String MC_ByteString
  -- | ServerBoundSpectate MC_UUID
  -- | ServerBoundResourcePackReceive MC_String MC_VarInt
  -- | ServerBoundUseItem MC_Hand MC_VarInt
  deriving (Show,Eq)


{-
instance Serialize ClientBoundPlay where
    {-
  put (ClientBoundKeepAlive keepAliveId) = do
    put (fromIntegral
      $ idLength
      + varIntLength keepAliveId :: Word8)
    put (0x1f :: Word8)
    putByteString keepAliveId
    -}
  put (ClientBoundLogin entityId gameMode dimension difficulty maxPlayers levelType reducedDebugInfo) = do
    put (fromIntegral
      $ 1
      + 4
      + 1
      + 1
      + 1
      + 1
      + 1 + stringLength levelType
      + 1 boolLength :: Word8)
    put (0x23 :: Word8)
    putWord32be entityId
    put gameMode
    put dimension
    put difficulty
    put maxPlayers
    put (stringLength levelType)
    putByteString levelType
    put reducedDebugInfo
    {-
  put (ClientBoundChat message position) = do
    put (fromIntegral
      $ idLength
      + stringLength message
      + byteLength :: Word8)
    put (0x0f :: Word8)
    putByteString message
    put position
    -}
    {-
  put (ClientBoundUpdateTime age time) = do
    put (fromIntegral
      $ idLength
      + (2 * intLength) :: Word8)
    put (0x43 :: Word8)
    putWord32be age
    putWord32be time
    -}
    {-
  put (ClientBoundEntityEquipment entityId slot item) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + varIntLength slot
      + slotLength item :: Word8)
    put (0x3c :: Word8)
    putByteString entityId
    putByteString slot
    putByteString item
    -}
    {-
  put (ClientBoundSpawnPosition location) = do
    put (fromIntegral
      $ idLength
      + positionLength :: Word8)
    put (0xce :: Word8)
    putWord64be location
    -}
    {-
  put (ClientBoundUpdateHealth health food foodSaturation) = do
    put (fromIntegral
      $ idLength
      + floatLength
      + varIntLength food
      + floatLength :: Word8)
    put (0x3e :: Word8)
    putWord32be health
    putByteString food
    putWord32be foodSaturation
    -}
    {-
  put (ClientBoundRespawn dimension difficulty gamemode levelType) = do
    put (fromIntegral
      $ idLength
      + intLength
      + ubyteLength
      + ubyteLength
      + stringLength levelType :: Word8)
    put (0x33 :: Word8)
    putWord32be dimension
    put difficulty
    put gamemode
    putByteString levelType
    -}
    {-
  put (ClientBoundPosition x y z yaw pitch flags) = do
    put (fromIntegral
      $ idLength
      + (3 * doubleLength)
      + (2 * floatLength)
      + byteLength :: Word8)
    put (0x2e :: Word8)
    putWord64be x
    putWord64be y
    putWord64be z
    putWord32be yaw
    putWord32be pitch
    put flags
    -}
    {-
  put (ClientBoundHeldItemSlot slot) = do
    put (fromIntegral
      $ idLength
      + byteLength :: Word8)
    put (0x37 :: Word8)
    put slot
    -}
    {-
  put (ClientBoundBed entityId location) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + positionLength :: Word8)
    put (0x2f :: Word8)
    putByteString entityId
    put location
    -}
    {-
  put (ClientBoundAnimation entityId animation) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + ubyteLength :: Word8)
    put (0x06 :: Word8)
    putByteString entityId
    put animation
    -}
    {-
  put (ClientBoundNamedEntitySpawn entityId playerUUID x y z yaw pitch metadata) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + uuidLength
      + (3 * intLength)
      + (2 * byteLength)
      + metadataLength metadata :: Word8)
    put (0x05 :: Word8)
    putByteString entityId
    putByteString playerUUID
    putWord32be x
    putWord32be y
    putWord32be z
    put yaw
    put pitch
    putByteString metadata
    -}
    {-
  put (ClientBoundCollect collectedEntityId collectorEntityId) = do
    put (fromIntegral
      $ idLength
      + varIntLength collectedEntityId
      + varIntLength collectorEntityId :: Word8)
    put (0x47 :: Word8)
    putByteString collectedEntityId
    putByteString collectorEntityId
    -}
    {-
  put (ClientBoundSpawnEntity entityId entityUUID entityType x y z pitch yaw intField velocityX velocityY velocityZ) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + uuidLength
      + byteLength
      + (3 * intLength)
      + (2 * byteLength)
      + intLength
      + (3 * shortLength) :: Word8)
    put (0x00 :: Word8)
    putByteString entityId
    putByteString entityUUID
    put entityType
    putWord32be x
    putWord32be y
    putWord32be z
    put pitch
    put yaw
    putWord32be intField
    putWord16be velocityX
    putWord16be velocityY
    putWord16be velocityZ
    -}
    {-
  put (ClientBoundSpawnEntityLiving entityId entityUUID entityType x y z yaw pitch headPitch velocityX velocityY velocityZ metadata) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + uuidLength
      + ubyteLength
      + (3 * intLength)
      + (3 * byteLength)
      + (3 * shortLength)
      + metadataLength metadata :: Word8)
    put (0x03 :: Word8)
    putByteString entityId
    putByteString entityUUID
    put entityType
    putWord32be x
    putWord32be y
    putWord32be z
    put yaw
    put pitch
    put headPitch
    putWord16be velocityX
    putWord16be velocityY
    putWord16be velocityZ
    putByteString metadata
    -}
    {-
  put (ClientBoundSpawnEntityPainting entityId title location direction) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + stringLength title
      + positionLength
      + ubyteLength :: Word8)
    put (0x04 :: Word8)
    putByteString entityId
    putByteString title
    putWord64be location
    put direction
    -}
    {-
  put (ClientBoundSpawnEntityExperienceOrb entityId x y z count) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + (3 * intLength)
      + shortLength :: Word8)
    put (0x01 :: Word8)
    putByteString entityId
    putWord32be x
    putWord32be y
    putWord32be z
    putWord16be count
    -}
    {-
  put (ClientBoundEntityVelocity entityId velocityX velocityY velocityZ) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + (3 * shortLength) :: Word8)
    put (0x3b :: Word8)
    putByteString entityId
    putWord16be velocityX
    putWord16be velocityY
    putWord16be velocityZ
    -}
    {-
  put ClientBoundEntityDestroy = undefined
    -}
    {-
  put (ClientBoundEntity entityId) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId :: Word8)
    put (0x29 :: Word8)
    putByteString entityId
    -}
    {-
  put (ClientBoundRelEntityMove entityId dX dY dZ onGround) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + (3 * byteLength)
      + boolLength :: Word8)
    put (0x26 :: Word8)
    putByteString entityId
    put dX
    put dY
    put dZ
    put onGround
    -}
    {-
  put (ClientBoundEntityLook entityId yaw pitch onGround) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + (2 * byteLength)
      + boolLength :: Word8)
    put (0x28 :: Word8)
    putByteString entityId
    put yaw
    put pitch
    put onGround
    -}
    {-
  put (ClientBoundEntityMoveLook entityId dX dY dZ yaw pitch onGround) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + (3 * byteLength)
      + (2 * byteLength)
      + boolLength :: Word8)
    put (0x27 :: Word8)
    putByteString entityId
    put dX
    put dY
    put dZ
    put yaw
    put pitch
    put onGround
    -}
    {-
  put (ClientBoundEntityTeleport entityId x y z yaw pitch onGround) = do
    put (fromIntegral
      $ idLength
      + (3 * intLength)
      + (2 * byteLength)
      + boolLength :: Word8)
    put (0x48 :: Word8)
    putByteString entityId
    putWord32be x
    putWord32be y
    putWord32be z
    put yaw
    put pitch
    put onGround
    -}
    {-
  put (ClientBoundEntityHeadRotation entityId headYaw) = do
    put (fromIntegral
      $ idLength
      + intLength
      + byteLength :: Word8)
    put (0x34 :: Word8)
    putByteString entityId
    put headYaw
    -}
    {-
  put (ClientBoundEntityStatus entityId entityStatus) = do
    put (fromIntegral
      $ idLength
      + intLength
      + byteLength :: Word8)
    put (0x1a :: Word8)
    putWord32be entityId
    put entityStatus
    -}
    {-
  put (ClientBoundAttachEntity entityId vehicleId leash) = do
    put (fromIntegral
      $ idLength
      + (2 * intLength)
      + boolLength :: Word8)
    put (0x3a :: Word8)
    putWord32be entityId
    putWord32be vehicleId
    put leash
    -}
    {-
  put (ClientBoundEntityMetadata entityId metadata) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + metadataLength metadata :: Word8)
    -}
    {-
  put (ClientBoundEntityEffect entityId effectId amplifier duration hideParticles) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + (2 * byteLength)
      + varIntLength duration
      + boolLength :: Word8)
    put (0x4a :: Word8)
    putByteString entityId
    put effectId
    put amplifier
    putByteString duration
    put hideParticles
    -}
    {-
  put (ClientBoundRemoveEntityEffect entityId effectId) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + byteLength :: Word8)
    put (0x31 :: Word8)
    putByteString entityId
    put effectId
    -}
    {-
  put (ClientBoundExperience experienceBar level totalExperience) = do
    put (fromIntegral
      $ idLength
      + floatLength
      + varIntLength level
      + varIntLength totalExperience :: Word8)
    put (0x3d :: Word8)
    putWord32be experienceBar
    putByteString level
    putByteString totalExperience
    -}
  -- put ClientBoundUpdateAttributes = undefined
  -- put ClientBoundMapChunk = undefined
  -- put ClientBoundMultiBlockChange = undefined
    {-
  put (ClientBoundBlockChange location blockType) = do
    put (fromIntegral
      $ idLength
      + positionLength
      + varIntLength blockType :: Word8)
    put (0x0b :: Word8)
    putWord64be location
    putByteString blockType
    -}
    {-
  put (ClientBoundBlockAction location byte1 byte2 blockID) = do
    put (fromIntegral
      $ idLength
      + positionLength
      + ubyteLength
      + ubyteLength
      + varIntLength blockID :: Word8)
    put (0x0a :: Word8)
    putWord64be location
    put byte1
    put byte2
    putByteString blockID
    -}
    {-
  put (ClientBoundBlockBreakAnimation entityId location destroyStage) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + positionLength
      + byteLength :: Word8)
    put (0x08 :: Word8)
    putByteString entityId
    putWord64be location
    put destroyStage
    -}
  -- put (ClientBoundExplosion _ _ _ _ _ _ _ _) = undefined
  -- put (ClientBoundWorldEvent effectId location eventData global) = do
    {-
    put (fromIntegral
      $ idLength
      + intLength
      + positionLength
      + intLength
      + boolLength :: Word8)
    put (0x21 :: Word8)
    putWord32be effectId
    putWord64be location
    putWord32be eventData
    put global
    -}
    {-
  put (ClientBoundNamedSoundEffect soundName x y z volume pitch) = do
    put (fromIntegral
      $ idLength
      + stringLength soundName
      + (3 * intLength)
      + floatLength
      + ubyteLength :: Word8)
    put (23 :: Word8)
    putByteString soundName
    putWord32be x
    putWord32be y
    putWord32be z
    putWord32be volume
    put pitch
    -}
  -- put ClientBoundWorldParticles = undefined
    {-
  put (ClientBoundGameStateChange reason gameMode) = do
    put (fromIntegral
      $ idLength
      + ubyteLength
      + floatLength :: Word8)
    put (0x1e :: Word8)
    put reason
    putWord32be gameMode
    -}
    {-
  put (ClientBoundSpawnEntityWeather entityId entityType x y z) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + byteLength
      + (3 * intLength) :: Word8)
    put (0x02 :: Word8)
    putByteString entityId
    put entityType
    putWord32be x
    putWord32be y
    putWord32be z
    -}
  -- put (ClientBoundOpenWindow _ _ _ _ _) = undefined
   {-
  put (ClientBoundCloseWindow windowId) = do
    put (fromIntegral
      $ idLength
      + ubyteLength :: Word8)
    put (0x16 :: Word8)
    put windowId
    -}
    {-
  put (ClientBoundSetSlot windowId slot item) = do
    put (fromIntegral
      $ idLength
      + byteLength
      + shortLength
      + slotLength item :: Word8)
    put (0x16 :: Word8)
    put windowId
    putWord16be slot
    putByteString item
    -}
  -- put ClientBoundWindowItems = undefined
    {-
  put (ClientBoundCraftProgressBar windowId property value) = do
    put (fromIntegral
      $ idLength
      + ubyteLength
      + (2 * shortLength) :: Word8)
    put (0x15 :: Word8)
    put windowId
    putWord16be property
    putWord16be value
    -}
    {-
  put (ClientBoundTransaction windowId action accepted) = do
    put (fromIntegral
      $ idLength
      + byteLength
      + shortLength
      + boolLength :: Word8)
    put (0x11 :: Word8)
    put windowId
    putWord16be action
    put accepted
    -}
    {-
  put (ClientBoundUpdateSign location text1 text2 text3 text4) = do
    put (fromIntegral
      $ idLength
      + positionLength
      + stringLength text1
      + stringLength text2
      + stringLength text3
      + stringLength text4 :: Word8)
    put (0x45 :: Word8)
    putWord64be location
    putByteString text1
    putByteString text2
    putByteString text3
    putByteString text4
    -}
  -- put ClientBoundMap = undefined
  -- put (ClientBoundTileEntityData _ _ _) = undefined
    {-
  put (ClientBoundOpenSignEntity location) = do
    put (fromIntegral
      $ idLength
      + positionLength :: Word8)
    put (0x2a :: Word8)
    putWord64be location
    -}
    {-
  put (ClientBoundStatistics statCount stats) = do
    put (fromIntegral
      $ idLength
      + 1
      + stringLength stats :: Word8)
    put (0x07 :: Word8)
    put statCount
    if (stringLength stats /= 0)
      then putByteString stats
      else return ()
    -}
  -- put ClientBoundPlayerInfo = undefined
    {-
  put (ClientBoundAbilities flags flyingSpeed walkingSpeed) = do
    put (fromIntegral
      $ idLength
      + byteLength
      + (2 * floatLength) :: Word8)
    put (0x2b :: Word8)
    put flags
    putWord32be flyingSpeed
    putWord32be walkingSpeed
    -}
  -- put ClientBoundTabComplete = undefined
  -- put ClientBoundScoreBoardObjective = undefined
  -- put ClientBoundScoreBoardScore = undefined
    {-
  put (ClientBoundScoreBoardDisplayObjective position objectiveName) = do
    put (fromIntegral
      $ idLength
      + byteLength
      + stringLength objectiveName :: Word8)
    put (0x38 :: Word8)
    put position
    putByteString objectiveName
    -}
  -- put ClientBoundScoreBoardTeam = undefined
    {-
  put (ClientBoundCustomPayload channel dat)= do
    put (fromIntegral
      $ idLength
      + 1 + stringLength channel
      + 1 + stringLength dat :: Word8)
    put (0x18 :: Word8)
    put (stringLength channel)
    putByteString channel
    put (stringLength dat)
    putByteString dat
    -}
    {-
  put (ClientBoundKickDisconnect reason) = do
    put (fromIntegral
      $ idLength
      + stringLength reason :: Word8)
    put (0x19 :: Word8)
    putByteString reason
    -}
    {-
  put (ClientBoundDifficulty d) = do
    put (fromIntegral
      $ idLength
      + ubyteLength :: Word8)
    put (0x0d :: Word8)
    put d
    -}
  -- put ClientBoundCombatEvent = undefined
    {-
  put (ClientBoundCamera cameraId) = do
    put (fromIntegral
      $ idLength
      + varIntLength cameraId :: Word8)
    put (0x36 :: Word8)
    putByteString cameraId
    -}
  -- put ClientBoundWorldBorder = undefined
  -- put ClientBoundTitle = undefined
    {-
  put (ClientBoundPlaySetCompression threshold) = do
    put (fromIntegral
      $ idLength
      + varIntLength threshold :: Word8)
    put (0x1d :: Word8)
    putByteString threshold
    -}
    {-
  put (ClientBoundPlayerlistHeader header footer) = do
    put (fromIntegral
      $ idLength
      + stringLength header
      + stringLength footer :: Word8)
    put (0x46 :: Word8)
    putByteString header
    putByteString footer
    -}
    {-
  put (ClientBoundResourcePackSend url hash) = do
    put (fromIntegral
      $ idLength
      + stringLength url
      + stringLength hash :: Word8)
    put (0x32 :: Word8)
    putByteString url
    putByteString hash
    -}
  -- put ClientBoundBossBar = undefined
    {-
  put (ClientBoundSetCooldown itemID cooldownTicks) = do
    put (fromIntegral
      $ idLength
      + varIntLength itemID
      + varIntLength cooldownTicks :: Word8)
    put (0x17 :: Word8)
    putByteString itemID
    putByteString cooldownTicks
    -}
    {-
  put (ClientBoundUnloadChunk chunkX chunkZ) = do
    put (fromIntegral
      $ idLength
      + (2 * intLength) :: Word8)
    put (0x1c :: Word8)
    putWord32be chunkX
    putWord32be chunkZ
    -}

  get = do
    _ <- getWord8
    packetID <- getWord8
    case packetID of
      {-
      0x00 -> return ClientBoundSpawnEntity
      0x01 -> return ClientBoundSpawnEntityExperienceOrb
      0x02 -> return ClientBoundSpawnEntityWeather
      0x03 -> return ClientBoundSpawnEntityLiving
      0x04 -> return ClientBoundSpawnEntityPainting
      0x05 -> return ClientBoundNamedEntitySpawn
      0x06 -> return ClientBoundAnimation
      -- 0x07 -> return ClientBoundStatistics
      0x08 -> return ClientBoundBlockBreakAnimation
      0x09 -> return ClientBoundTileEntityData
      -- 0x0c -> return ClientBoundBossBar
      0x0d -> return ClientBoundDifficulty
      -- 0x0e -> return ClientBoundTabComplete
      0x11 -> return ClientBoundTransaction
      0x12 -> return ClientBoundCloseWindow
      0x13 -> return ClientBoundOpenWindow
      -- 0x14 -> return ClientBoundWindowItems
      0x15 -> return ClientBoundCraftProgressBar
      0x16 -> return ClientBoundSetSlot
      0x17 -> return ClientBoundSetCooldown
      -- 0x18 -> return ClientBoundCustomPayload
      0x19 -> return ClientBoundKickDisconnect
      0x0a -> return ClientBoundBlockAction
      0x0b -> return ClientBoundBlockChange
      0x0f -> undefined -- ClientBoundChat
      -- 0x10 -> return ClientBoundMultiBlockChange
      0x1a -> return ClientBoundEntityStatus
      0x1b -> return ClientBoundExplosion
      0x1c -> return ClientBoundUnloadChunk
      0x1d -> return ClientBoundPlaySetCompression
      0x1e -> return ClientBoundGameStateChange
      0x1f -> undefined -- ClientBoundKeepAlive
      -- 0x20 -> return ClientBoundMapChunk
      0x21 -> return ClientBoundWorldEvent
      -- 0x22 -> return ClientBoundWorldParticles
      0x23 -> undefined -- ClientBoundNamedSoundEffect
      0x24 -> undefined -- ClientBoundLogin
      -- 0x25 -> return ClientBoundMap
      0x26 -> return ClientBoundRelEntityMove
      0x27 -> return ClientBoundEntityMoveLook
      0x28 -> return ClientBoundEntityLook
      0x29 -> return ClientBoundEntity
      0x2a -> return ClientBoundOpenSignEntity
      0x2b -> return ClientBoundAbilities
      -- 0x2c -> return ClientBoundCombatEvent
      -- 0x2d -> return ClientBoundPlayerInfo
      0x2e -> return ClientBoundPosition
      0x2f -> return ClientBoundBed
      -- 0x30 -> return ClientBoundEntityDestroy
      0x31 -> return ClientBoundRemoveEntityEffect
      0x32 -> return ClientBoundResourcePackSend
      0x33 -> return ClientBoundRespawn
      0x34 -> return ClientBoundEntityHeadRotation
      -- 0x35 -> return ClientBoundWorldBorder
      0x36 -> return ClientBoundCamera
      0x37 -> return ClientBoundHeldItemSlot
      0x38 -> return ClientBoundScoreBoardDisplayObjective
      0x39 -> return ClientBoundEntityMetadata
      0x3a -> return ClientBoundAttachEntity
      0x3b -> return ClientBoundEntityVelocity
      0x3c -> undefined -- ClientBoundEntityEquipment
      0x3d -> return ClientBoundExperience
      0x3e -> return ClientBoundUpdateHealth
      -- 0x3f -> return ClientBoundScoreBoardObjective
      -- 0x40 -> return ClientBoundScoreBoardTeam
      -- 0x41 -> return ClientBoundScoreBoardScore
      0x42 -> ClientBoundSpawnPosition
                <$> getWord64be
      0x43 -> ClientBoundUpdateTime
                <$> getWord64be
                <*> getWord64be
      -- 0x44 -> return ClientBoundTitle
      0x45 -> return ClientBoundUpdateSign
      0x46 -> return ClientBoundPlayerlistHeader
      0x47 -> return ClientBoundCollect
      0x48 -> return ClientBoundEntityTeleport
      -- 0x49 -> return ClientBoundUpdateAttributes
      0x4a -> return ClientBoundEntityEffect
      -}
      _    -> undefined


instance Serialize ServerBoundPlay where
    {-
  put (ServerBoundKeepAlive keepAliveId) = do
    put (fromIntegral
      $ idLength
      + varIntLength keepAliveId :: Word8)
    put (0x0a :: Word8)
    putByteString keepAliveId
    -}
    {-
  put (ServerBoundChat message) = do
    put (fromIntegral
      $ idLength
      + stringLength message :: Word8)
    put (0x01 :: Word8)
    putByteString message
    -}
  -- put ServerBoundUseEntity = undefined
    {-
  put (ServerBoundFlying onGround) = do
    put (fromIntegral
      $ idLength
      + boolLength :: Word8)
    put (0x0e :: Word8)
    put onGround
    -}
    {-
  put (ServerBoundPosition x y z onGround) = do
    put (fromIntegral
      $ idLength
      + (3 * doubleLength)
      + boolLength :: Word8)
    put (0x0b :: Word8)
    putWord64be x
    putWord64be y
    putWord64be z
    put onGround
    -}
    {-
  put (ServerBoundLook yaw pitch onGround) = do
    put (fromIntegral
      $ idLength
      + floatLength
      + floatLength
      + boolLength :: Word8)
    put (0x0d :: Word8)
    putWord32be yaw
    putWord32be pitch
    put onGround
    -}
    {-
  put (ServerBoundPositionLook x y z yaw pitch onGround) = do
    put (fromIntegral
      $ idLength
      + (3 * doubleLength)
      + floatLength
      + floatLength
      + boolLength :: Word8)
    put (0x0c :: Word8)
    putWord64be x
    putWord64be y
    putWord64be z
    putWord32be yaw
    putWord32be pitch
    put onGround
    -}
    {-
  put (ServerBoundBlockDig status location face) = do
    put (fromIntegral
      $ idLength
      + byteLength
      + positionLength
      + byteLength :: Word8)
    put (0x10 :: Word8)
    put status
    putWord64be location
    put face
    -}
    {-
  put (ServerBoundBlockPlace location direction hand cursorX cursorY cursorZ) = do
    put (fromIntegral
      $ idLength
      + positionLength
      + varIntLength direction
      + varIntLength hand
      + (3 * byteLength) :: Word8)
    put (0x19 :: Word8)
    putWord64be location
    putByteString direction
    putByteString hand
    put cursorX
    put cursorY
    put cursorZ
    -}
    {-
  put (ServerBoundHeldItemSlot slotId) = do
    put (fromIntegral
      $ idLength
      + shortLength :: Word8)
    put (0x14 :: Word8)
    putWord16be slotId
    -}
    {-
  put (ServerBoundArmAnimation hand) = do
    put (fromIntegral
      $ idLength
      + varIntLength hand :: Word8)
    put (0x17 :: Word8)
    putByteString hand
    -}
    {-
  put (ServerBoundEntityAction entityId actionId jumpBoost) = do
    put (fromIntegral
      $ idLength
      + varIntLength entityId
      + varIntLength actionId
      + varIntLength jumpBoost :: Word8)
    put (0x11 :: Word8)
    putByteString entityId
    putByteString actionId
    putByteString jumpBoost
    -}
    {-
  put (ServerBoundSteerVehicle sideways forward jump) = do
    put (fromIntegral
      $ idLength
      + floatLength
      + floatLength
      + ubyteLength :: Word8)
    put (0x12 :: Word8)
    putWord32be sideways
    putWord32be forward
    put jump
    -}
    {-
  put (ServerBoundCloseWindow windowId) = do
    put (fromIntegral
      $ idLength
      + ubyteLength :: Word8)
    put (0x07 :: Word8)
    put windowId
    -}
    {-
  put (ServerBoundWindowClick windowId slot mouseButton action mode item) = do
    put (fromIntegral
      $ idLength
      + ubyteLength
      + shortLength
      + byteLength
      + shortLength
      + byteLength
      + slotLength item :: Word8)
    put (0x06 :: Word8)
    put windowId
    putWord16be slot
    put mouseButton
    putWord16be action
    put mode
    putByteString item
    -}
    {-
  put (ServerBoundTransaction windowId action accepted) = do
    put (fromIntegral
      $ idLength
      + byteLength
      + shortLength
      + boolLength :: Word8)
    put (0x04 :: Word8)
    put windowId
    putWord16be action
    put accepted
    -}
    {-
  put (ServerBoundSetCreativeSlot slot item) = do
    put (fromIntegral
      $ idLength
      + shortLength
      + slotLength item :: Word8)
    put (0x15 :: Word8)
    putWord16be slot
    putByteString item
    -}
    {-
  put (ServerBoundEnchantItem windowId enchantment) = do
    put (fromIntegral
      $ idLength
      + (2 * byteLength) :: Word8)
    put (0x05 :: Word8)
    put windowId
    put enchantment
    -}
    {-
  put (ServerBoundUpdateSign location text1 text2 text3 text4) = do
    put (fromIntegral
      $ idLength
      + positionLength
      + stringLength text1
      + stringLength text2
      + stringLength text3
      + stringLength text4 :: Word8)
    put (0x16 :: Word8)
    putWord64be location
    putByteString text1
    putByteString text2
    putByteString text3
    putByteString text4
    -}
    {-
  put (ServerBoundAbilities flags flyingSpeed walkingSpeed) = do
    put (fromIntegral
      $ idLength
      + byteLength
      + floatLength
      + floatLength :: Word8)
    put (0x0f :: Word8)
    put flags
    putWord32be flyingSpeed
    putWord32be walkingSpeed
    -}
    {-
  put (ServerBoundTabComplete _ _) = undefined
    -}
    {-
  put (ServerBoundSettings locale viewDistance chatFlags chatColors skinParts mainHand) = do
    put (fromIntegral
      $ idLength
      + stringLength locale
      + byteLength
      + varIntLength chatFlags
      + boolLength
      + ubyteLength
      + varIntLength mainHand :: Word8)
    put (0x03 :: Word8)
    putByteString locale
    put viewDistance
    putByteString chatFlags
    put chatColors
    put skinParts
    putByteString mainHand
    -}
    {-
  put (ServerBoundClientCommand payload) = do
    put (fromIntegral
      $ idLength
      + varIntLength payload :: Word8)
    put (0x02 :: Word8)
    putByteString payload
    -}
    {-
  put ServerBoundCustomPayload = undefined
    -}
    {-
  put (ServerBoundSpectate target) = do
    put (fromIntegral
      $ idLength
      + uuidLength :: Word8)
    put (0x18 :: Word8)
    putByteString target
    -}
    {-
  put (ServerBoundResourcePackReceive hash result) = do
    put (fromIntegral
      $ idLength
      + stringLength hash
      + (varIntLength result) :: Word8)
    put (0x13 :: Word8)
    putByteString hash
    putByteString result
    -}
    {-
  put ServerBoundUseItem = undefined
    -}
{-
  get = do
    _ <- getWord8
    packetID <- getWord8
    case packetID of
      {-
      -- 0x00 -> return ServerBoundTabComplete
      0x01 -> return ServerBoundChat
      0x02 -> return ServerBoundClientCommand
      0x03 -> return ServerBoundSettings
      0x04 -> return ServerBoundTransaction
      0x05 -> return ServerBoundEnchantItem
      0x06 -> return ServerBoundWindowClick
      0x07 -> return ServerBoundCloseWindow
      -- 0x08 -> return ServerBoundCustomPayload
      -- 0x09 -> return ServerBoundUseEntity
      0x0a -> return ServerBoundKeepAlive
      0x0b -> return ServerBoundPosition
      0x0c -> return ServerBoundPositionLook
      0x0d -> return ServerBoundLook
      0x0e -> return ServerBoundFlying
      0x0f -> return ServerBoundAbilities
      0x10 -> return ServerBoundBlockDig
      0x11 -> return ServerBoundEntityAction
      0x12 -> return ServerBoundSteerVehicle
      0x13 -> return ServerBoundResourcePackReceive
      0x14 -> return ServerBoundHeldItemSlot
      0x15 -> return ServerBoundSetCreativeSlot
      0x16 -> return ServerBoundUpdateSign
      0x17 -> return ServerBoundArmAnimation
      0x18 -> return ServerBoundSpectate
      0x19 -> return ServerBoundBlockPlace
      -- 0x1a -> return ServerBoundUseItem
      -}
      _    -> undefined
-}
-}


data StatusPayload = StatusPayload
  { version       :: Version
  , players       :: Players
  , description   :: Description
  } deriving (Generic,Show,Eq,Read)


instance Aeson.ToJSON StatusPayload
instance Aeson.FromJSON StatusPayload


data Version = Version
  { name      :: T.Text
  , protocol  :: Int
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Version
instance Aeson.FromJSON Version


data Players = Players
  { max     :: Int
  , online  :: Int
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Players
instance Aeson.FromJSON Players


data Description = Description
  { text    :: T.Text
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Description
instance Aeson.FromJSON Description
