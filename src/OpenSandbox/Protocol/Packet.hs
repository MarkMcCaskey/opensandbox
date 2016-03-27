{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Packet
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Packet
  ( ClientBoundStatus (..)
  , ServerBoundStatus (..)
  , ClientBoundLogin (..)
  , ServerBoundLogin (..)
  , ClientBoundPlay (..)
  , ServerBoundPlay (..)
  ) where

import            Prelude hiding (max)
import qualified  Data.Aeson as Aeson
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Char8 as BC
import qualified  Data.ByteString.Lazy as BL
import            Data.Int
import            Data.Maybe
import            Data.NBT
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.Serialize
import            Data.UUID
import qualified  Data.Vector as V
import            Data.Word

import            OpenSandbox.Types
import            OpenSandbox.Protocol.Types


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
  = ClientBoundLoginDisconnect B.ByteString
  | ClientBoundEncryptionRequest B.ByteString B.ByteString B.ByteString
  | ClientBoundLoginSuccess B.ByteString B.ByteString
  | ClientBoundSetCompression Word16
  deriving (Show,Eq)


instance Serialize ClientBoundLogin where
  put (ClientBoundLoginDisconnect reason) = do
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
      0 -> ClientBoundLoginDisconnect
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


data ClientBoundPlay
  = ClientBoundSpawnObject Int UUID Word8 Double Double Double Angle Angle Int Short Short Short
  | ClientBoundSpawnExperienceOrb Int Double Double Double Short
  | ClientBoundSpawnGlobalEntity Int Word8 Double Double Double
  | ClientBoundSpawnMob Int UUID Word8 Double Double Double Angle Angle Angle Short Short Short EntityMetadata
  | ClientBoundSpawnPainting Int UUID String Position Word8
  | ClientBoundSpawnPlayer Int UUID Double Double Double Angle Angle EntityMetadata
  | ClientBoundAnimation Int Animation
  | ClientBoundStatistics (V.Vector Stat)
  | ClientBoundBlockBreakAnimation Int Position Word8
  | ClientBoundUpdateBlockEntity Position Word8 (Maybe NBT)
  | ClientBoundBlockAction Position Word8 Word8 Int
  | ClientBoundBlockChange Position Int
  | ClientBoundBossBar UUID Int BossBarAction
  | ClientBoundDifficulty Difficulty
  | ClientBoundTabComplete (V.Vector T.Text)
  | ClientBoundChatMessage Chat Word8
  | ClientBoundMultiBlockChange Int Int Int (V.Vector BlockChange)
  | ClientBoundConfirmTransaction Word8 Short Bool
  | ClientBoundCloseWindow Word8
  | ClientBoundOpenWindow Word8 T.Text Chat Word8 (Maybe Int32)
  | ClientBoundWindowItems Word8 Short (V.Vector Slot)
  | ClientBoundWindowProperty Word8 Short Short
  | ClientBoundSetSlot Word8 Short Short
  | ClientBoundSetCooldown Int Int
  | ClientBoundPluginMessage T.Text B.ByteString
  | ClientBoundNamedSoundEffect T.Text Int Int32 Int32 Int32 Float Word8
  | ClientBoundPlayDisconnect Chat
  | ClientBoundEntityStatus Int32 Word8
  | ClientBoundExplosion Float Float Float Float Int32 B.ByteString Float Float Float
  | ClientBoundUnloadChunk Int32 Int32
  | ClientBoundChangeGameState Word8 Float
  | ClientBoundKeepAlive Int
  | ClientBoundChunkData Int32 Int32 Bool Int Int (V.Vector ChunkSection) (Maybe B.ByteString)
  | ClientBoundEffect Int32 Position Int32 Bool
  | ClientBoundParticle Int32 Bool Float Float Float Float Float Float Float Int32 (V.Vector Int)
  | ClientBoundJoinGame Int32 Word8 Word8 Word8 Word8 T.Text Bool
  | ClientBoundMap Int Word8 Bool (V.Vector Icon) Word8 (Maybe Word8) (Maybe Word8) (Maybe Word8) (Maybe Int) (Maybe B.ByteString)
  | ClientBoundEntityRelativeMove Int Short Short Short Bool
  | ClientBoundEntityLookAndRelativeMove Int Short Short Short Angle Angle Bool
  | ClientBoundLook Int Angle Angle Bool
  | ClientBoundEntity Int
  | ClientBoundVehicleMove Double Double Double Float Float
  | ClientBoundOpenSignEditor Position
  | ClientBoundPlayerAbilities Word8 Float Float
  | ClientBoundCombatEvent CombatEvent
  | ClientBoundPlayerListItem Int (V.Vector Player)
  | ClientBoundPlayerPositionAndLook Double Double Double Float Float Word8 Int
  | ClientBoundUseBed Int Position
  | ClientBoundDestroyEntities (V.Vector Int)
  | ClientBoundRemoveEntityEffect Int Word8
  | ClientBoundResourcePackSend T.Text B.ByteString
  | ClientBoundRespawn Dimension Difficulty GameMode T.Text
  | ClientBoundEntityHeadLook Int Angle
  | ClientBoundWorldBorder WorldBorderAction
  | ClientBoundCamera Int
  | ClientBoundHeldItemChange Word8
  | ClientBoundDisplayScoreboard Word8 T.Text
  | ClientBoundEntityMetadata Int EntityMetadata
  | ClientBoundAttachEntity Int32 Int32
  | ClientBoundEntityVelocity Int Short Short Short
  | ClientBoundEntityEquipment Int Int Slot
  | ClientBoundSetExperience Float Int Int
  | ClientBoundUpdateHealth Float Int Float
  | ClientBoundScoreboardObjective T.Text Word8 (Maybe T.Text) (Maybe T.Text)
  | ClientBoundSetPassengers Int Int (V.Vector Int)
  | ClientBoundTeams T.Text Word8 TeamMode
  | ClientBoundUpdateScore T.Text Word8 T.Text (Maybe Int)
  | ClientBoundSpawnPosition Position
  | ClientBoundTimeUpdate Int64 Int64
  | ClientBoundTitle Int TitleAction
  | ClientBoundUpdateSign Position Chat Chat Chat Chat
  | ClientBoundSoundEffect Int Int Int32 Int32 Int32 Float Word8
  | ClientBoundPlayerListHeaderAndFooter Chat Chat
  | ClientBoundCollectItem Int Int
  | ClientBoundEntityTeleport Int Double Double Double Angle Angle Bool
  | ClientBoundEntityProperties Int (V.Vector EntityProperty)
  | ClientBoundEntityEffect Int Word8 Word8 Int Word8

  | ClientBoundCustomPayload T.Text T.Text
  | ClientBoundLogin Int32 GameMode Dimension Difficulty Word8 WorldType Bool
  | ClientBoundHeldItemSlot Bool
  | ClientBoundUpdateTime Int64 Int64
  deriving (Show,Eq)


instance Serialize ClientBoundPlay where
  put (ClientBoundSpawnObject entityId objectUUID objectType x y z pitch yaw dat vX vY vZ) = undefined
    -- 0x00
  put (ClientBoundSpawnExperienceOrb entityId x y z count) = undefined
    -- 0x01
  put (ClientBoundSpawnGlobalEntity entityId entityType x y z) = undefined
    -- 0x02
  put (ClientBoundSpawnMob entityId entityUUID entityType x y z yaw pitch headPitch vX vY vZ metadata) = undefined
    -- 0x03
  put (ClientBoundSpawnPainting entityId entityUUID title location direction) = undefined
    -- 0x04
  put (ClientBoundSpawnPlayer entityId playerUUID x y z yaw pitch metadata) = undefined
    -- 0x05
  put (ClientBoundAnimation entityId animation) = undefined
    -- 0x06
  put (ClientBoundStatistics stats) = do
    let statPayload = encode stats
    let statLen = B.length statPayload
    putVarInt $ 1 + statLen
    putWord8 0x07
    putByteString statPayload
  put (ClientBoundBlockBreakAnimation entityId location destroyStage) = undefined
  put (ClientBoundUpdateBlockEntity location action nbtData) = undefined
  put (ClientBoundBlockAction location byte1 byte2 blockType) = undefined
  put (ClientBoundBlockChange location blockId) = undefined
  put (ClientBoundBossBar uuid action actionPayload) = undefined
  put (ClientBoundDifficulty d) = do
    putVarInt 2
    putWord8 0x0d
    putWord8 . toEnum . fromEnum $ d
  put (ClientBoundTabComplete matches) = undefined
  put (ClientBoundCustomPayload channel dat)= do
    let len1 = (B.length . encodeUtf8 $ channel)
    let len2 = (B.length . encodeUtf8 $ dat)
    putVarInt $ 1 + 1 + len1 + 1 + len2
    putWord8 0x18
    putVarInt len1
    putByteString $ encodeUtf8 channel
    putVarInt len2
    putByteString $ encodeUtf8 dat
  put (ClientBoundEntityStatus entityID entityStatus) = do
    putVarInt 6
    putWord8 0x1b
    putWord32be . toEnum . fromEnum $ entityID
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
    putVarInt $ 1 + (B.length . encode $ action) + (B.length . encode $ players)
    putWord8 0x2d
    put action
    put players
  put (ClientBoundHeldItemSlot slot) = do
    putVarInt 2
    putWord8 0x37
    put slot
  put (ClientBoundUpdateTime age time) = do
    putWord8 0x09
    putWord8 0x43
    putWord32be . toEnum . fromEnum $ age
    putWord32be . toEnum . fromEnum $ time

  get = do
    _ <- getVarInt
    packetID <- getWord8
    case packetID of
      0x00 -> return undefined -- ClientBoundSpawnObject entityId objectUUID objectType x y z pitch yaw dat vX vY vZ
      0x01 -> return undefined -- ClientBoundSpawnExperienceOrb entityId x y z count
      0X02 -> return undefined -- ClientBoundGlobalEntity entityId entityType x y z
      0x03 -> return undefined  -- ClientBoundSpawnMob entityId entityUUID entityType x y z yaw pitch headPitch vX vY vZ metadata
      0x04 -> return undefined -- ClientBoundSpawnPainting entityId entityUUID title location direction
      0x05 -> return undefined -- ClientBoundSpawnPlayer entityId playerUUID x y z yaw pitch metadata
      0x06 -> return undefined -- ClientBoundAnimation entityId animation
      0x07 -> return undefined -- ClientBoundStatistics
      0x08 -> return undefined -- ClientBoundBlockBreakAnimation entityId location destroyStage
      0x09 -> return undefined -- ClientBoundUpdateBlockEntity location action nbtData
      0x0a -> return undefined -- ClientBoundBlockAction location byte1 byte2 blockType
      0x0b -> return undefined -- ClientBoundBlockChange location blockId
      0x0c -> return undefined -- ClientBoundBossBar uuid action actionPayload
      0x0d -> ClientBoundDifficulty
                <$> (fmap (toEnum . fromEnum) getWord8)
      0x0e -> return undefined -- ClientBoundTabComplete matches

      0x18 -> ClientBoundCustomPayload
                <$> fmap decodeUtf8 (getVarInt >>= getByteString)
                <*> fmap decodeUtf8 (getVarInt >>= getByteString)
      0x1b -> ClientBoundEntityStatus
                <$> (fmap (toEnum . fromEnum) getWord32be)
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
  = ServerBoundTeleportConfirm Int
  | ServerBoundTabComplete T.Text Bool Bool (Maybe Position)
  | ServerBoundChatMessage T.Text
  | ServerBoundClientStatus Int
  | ServerBoundClientSettings T.Text Word8 Int Bool Word8 Int
  | ServerBoundConfirmTransaction Word8 Short Bool
  | ServerBoundEnchantItem Word8 Word8
  | ServerBoundClickWindow Word8 Short Word8 Short Int Slot
  | ServerBoundCloseWindow Word8
  | ServerBoundPluginMessage T.Text B.ByteString
  | ServerBoundUseEntity Int Int (Maybe Float) (Maybe Float) (Maybe Float) (Maybe Int)
  | ServerBoundKeepAlive Int
  | ServerBoundPlayerPosition Double Double Double Bool
  | ServerBoundPlayerPositionAndLook Double Double Double Float Float Bool
  | ServerBoundPlayerLook Float Float Bool
  | ServerBoundPlayer Bool
  | ServerBoundVehicleMove Double Double Double Float Float
  | ServerBoundSteerBoat Bool Bool
  | ServerBoundPlayerAbilities Word8 Float Float
  | ServerBoundPlayerDigging Int Position Word8
  | ServerBoundEntityAction Int Int Int
  | ServerBoundSteerVehicle Float Float Word8
  | ServerBoundResourcePackStatus B.ByteString Int
  | ServerBoundHeldItemChange Short
  | ServerBoundCreativeInventoryAction Short Slot
  | ServerBoundUpdateSign Position T.Text T.Text T.Text T.Text
  | ServerBoundAnimation Int
  | ServerBoundSpectate UUID
  | ServerBoundPlayerBlockPlacement Position Int Int Word8 Word8 Word8
  | ServerBoundUseItem Int
  deriving (Show,Eq)


instance Serialize ServerBoundPlay where
  put (ServerBoundKeepAlive payload) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x0b
      putVarInt payload
  put (ServerBoundTeleportConfirm teleportId) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x00
      putVarInt teleportId
  put (ServerBoundTabComplete text assumeCmd hasPosition lookedAtBlock) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x01
      putText text
      putBool assumeCmd
      putBool hasPosition
      if hasPosition
        then putPosition (fromJust lookedAtBlock)
        else return ()
  put (ServerBoundChatMessage message) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x02
      putText message
  put (ServerBoundClientStatus actionId) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x03
      putVarInt actionId
  put (ServerBoundClientSettings locale viewDistance chatMode chatColors skinParts mainHand) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x04
      putText locale
      putWord8 viewDistance
      putVarInt chatMode
      putBool chatColors
      putWord8 skinParts
      putVarInt mainHand
  put (ServerBoundConfirmTransaction windowId actionNum accepted) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x05
      putWord8 windowId
      putWord16be actionNum
      putBool accepted
  put (ServerBoundEnchantItem windowId enchantment) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x06
      putWord8 windowId
      putWord8 enchantment
  put (ServerBoundClickWindow windowId slot button actionNum mode clickedItem) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x07
      putWord8 windowId
      putWord16be slot
      putWord8 button
      putWord16be actionNum
      putVarInt mode
      putSlot clickedItem
  put (ServerBoundCloseWindow windowId) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x08
      putWord8 windowId
  put (ServerBoundPluginMessage channel dat) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x09
      putText channel
      putByteString dat
  put (ServerBoundUseEntity target useType tX tY tZ hand) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x0a
      putVarInt target
      putVarInt useType
      case useType of
        0 -> putVarInt $ fromJust hand
        1 -> return ()
        2 -> do
          putFloat32be . fromJust $ tX
          putFloat32be . fromJust $ tY
          putFloat32be . fromJust $ tZ
          putVarInt . fromJust $ hand
        _ -> undefined
  put (ServerBoundKeepAlive keepAliveId) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x0b
      putVarInt keepAliveId
  put (ServerBoundPlayerPosition x feetY z onGround) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x0c
  put (ServerBoundPlayerPositionAndLook x feetY z yaw pitch onGround) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x0d
      putFloat64be x
      putFloat64be feetY
      putFloat64be z
      putFloat32be yaw
      putFloat32be pitch
      putBool onGround
  put (ServerBoundPlayerLook yaw pitch onGround) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x0e
      putFloat32be yaw
      putFloat32be pitch
      putBool onGround
  put (ServerBoundPlayer onGround) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x0f
      putBool onGround
  put (ServerBoundVehicleMove x y z yaw pitch) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x10
      putFloat64be x
      putFloat64be y
      putFloat64be z
      putFloat32be yaw
      putFloat32be pitch
  put (ServerBoundSteerBoat unknown1 unknown2) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x11
      putBool unknown1
      putBool unknown2
  put (ServerBoundPlayerAbilities flags flyingSpeed walkingSpeed) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x12
      putWord8 flags
      putFloat32be flyingSpeed
      putFloat32be walkingSpeed
  put (ServerBoundPlayerDigging status location face) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x13
      putVarInt status
      putPosition location
      putWord8 face
  put (ServerBoundEntityAction entityId actionId jumpBoost) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x14
      putVarInt entityId
      putVarInt actionId
      putVarInt jumpBoost
  put (ServerBoundSteerVehicle sideways forward flags) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x15
      putFloat32be sideways
      putFloat32be forward
      putWord8 flags
  put (ServerBoundResourcePackStatus hash result) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x16
      putByteString hash
      putVarInt result
  put (ServerBoundHeldItemChange slot) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x17
      putWord16be slot
  put (ServerBoundCreativeInventoryAction slot clickedItem) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x18
      putWord16be slot
      putSlot clickedItem
  put (ServerBoundUpdateSign location line1 line2 line3 line4) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x19
      putPosition location
      putText line1
      putText line2
      putText line3
      putText line4
  put (ServerBoundAnimation hand) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x1a
      putVarInt hand
  put (ServerBoundSpectate targetPlayer) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x1b
      putUUID targetPlayer
  put (ServerBoundPlayerBlockPlacement location face hand x y z) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x1c
      putPosition location
      putVarInt face
      putVarInt hand
      putWord8 x
      putWord8 y
      putWord8 z
  put (ServerBoundUseItem hand) = do
    putVarInt $ B.length $ runPut $ do
      putWord8 0x1d
      putVarInt hand

  get = do
    _ <- getVarInt
    packetID <- getWord8
    case packetID of
      0x0b -> ServerBoundKeepAlive
                <$> getVarInt
      _ -> undefined
