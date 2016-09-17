{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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
  ( SBHandshaking (..)
  , CBStatus (..)
  , SBStatus (..)
  , CBLogin (..)
  , SBLogin (..)
  , CBPlay (..)
  , SBPlay (..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import Data.Int
import qualified Data.Text as T
import Data.Serialize
import Data.UUID
import qualified Data.Vector as V
import Data.Word
import OpenSandbox.Protocol.Types
import OpenSandbox.World
import Prelude hiding (max)

data SBHandshaking
  = SBHandshake VarInt T.Text Short ProtocolState
  | SBLegacyServerListPing
  deriving (Show,Eq)

instance Serialize SBHandshaking where
  put (SBHandshake protocolVersion srvAddress srvPort nextState) = do
    putWord8 0x00
    putVarInt protocolVersion
    putText srvAddress
    put srvPort
    put nextState
  put SBLegacyServerListPing = do
    putWord8 0xfe
    putWord8 0x01

  get = do
    packetID <- getWord8
    case packetID of
      0x00 -> SBHandshake <$> getVarInt <*> getText <*> getInt16be <*> get
      0xfe -> do
        _ <- getWord8
        ln <- remaining
        _ <- getBytes ln
        return SBLegacyServerListPing
      err -> fail $ "Error: Unrecognized packetID: " ++ show err

data CBStatus
  = CBResponse T.Text Word8 Word8 Word8 T.Text
  | CBPong Int64
  deriving (Show,Eq)

instance Serialize CBStatus where
  put (CBResponse mvVersion versionID currentPlayers maxPlayers motd) = do
    putWord8 0x00
    encodeStatusPayload mvVersion versionID currentPlayers maxPlayers motd
  put (CBPong payload) = do
    putWord8 0x01
    put payload

  get = do
    packetID <- getWord8
    case packetID of
      0x00 -> do
        rawJSON <- getNetcodeByteString
        let eitherJSON = Aeson.eitherDecodeStrict rawJSON
        case eitherJSON of
          Left err -> fail err
          Right json -> return $
            CBResponse
              (name . version $ json)
              (protocol . version $ json)
              (online . players $ json)
              (max . players $ json)
              (text . description $ json)
      0x01 -> CBPong <$> getInt64be
      err -> fail $ "Error: Unrecognized packetID: " ++ show err

data SBStatus
  = SBRequest
  | SBPing Int64
  deriving (Show,Eq)

instance Serialize SBStatus where
  put SBRequest = putWord8 0x00
  put (SBPing payload) = do
    putWord8 0x01
    put payload
  get = do
    packetID <- getWord8
    case packetID of
      0x00 -> return SBRequest
      0x01 -> SBPing <$> getInt64be
      err -> fail $ "Error: Unrecognized packetID: " ++ show err

data CBLogin
  = CBLoginDisconnect T.Text
  | CBEncryptionRequest T.Text B.ByteString B.ByteString
  | CBLoginSuccess UUID T.Text
  | CBSetCompression VarInt
  deriving (Show,Eq)

instance Serialize CBLogin where
  put (CBLoginDisconnect reason) = do
    putWord8 0x00
    putText reason
  put (CBEncryptionRequest srvID publicKey verifyToken) = do
    putWord8 0x01
    putText srvID
    putNetcodeByteString publicKey
    putNetcodeByteString verifyToken
  put (CBLoginSuccess uuid username) = do
    putWord8 0x02
    putUUID uuid
    putText username
  put (CBSetCompression threshold) = do
    putWord8 0x03
    putVarInt threshold

  get = do
    packetID <- getWord8
    case packetID of
      0x00 -> CBLoginDisconnect <$> getText
      0x01 -> do
        srvID <- getText
        publicKey <- getNetcodeByteString
        verifyToken <- getNetcodeByteString
        return $ CBEncryptionRequest srvID publicKey verifyToken
      0x02 -> CBLoginSuccess <$> getUUID <*> getText
      0x03 -> CBSetCompression <$> getVarInt
      err -> fail $ "Error: Unrecognized packetID: " ++ show err

data SBLogin
  = SBLoginStart T.Text
  | SBEncryptionResponse B.ByteString B.ByteString
  deriving (Show,Eq)

instance Serialize SBLogin where
  put (SBLoginStart name) = do
    putWord8 0x00
    putText name
  put (SBEncryptionResponse sharedSecret verifyToken) = do
    putWord8 0x01
    putNetcodeByteString sharedSecret
    putNetcodeByteString verifyToken

  get = do
    packetID <- getWord8
    case packetID of
      0x00 -> SBLoginStart <$> getText
      0x01 -> SBEncryptionResponse <$> getNetcodeByteString <*> getNetcodeByteString
      err -> fail $ "Error: Unrecognized packetID: " ++ show err

data CBPlay
  = CBSpawnObject VarInt UUID Int8 Double Double Double Angle Angle Int32 Short Short Short
  | CBSpawnExperienceOrb VarInt Double Double Double Int16
  | CBSpawnGlobalEntity VarInt Int8 Double Double Double
  | CBSpawnMob VarInt UUID Word8 Double Double Double Angle Angle Angle Short Short Short EntityMetadata
  | CBSpawnPainting VarInt UUID T.Text Position Int8
  | CBSpawnPlayer VarInt UUID Double Double Double Angle Angle EntityMetadata
  | CBAnimation VarInt Animation
  | CBStatistics (V.Vector Statistic)
  | CBBlockBreakAnimation VarInt Position Int8
  | CBUpdateBlockEntity Position UpdateBlockEntityAction NBT
  | CBBlockAction Position BlockAction
  | CBBlockChange Position VarInt
  | CBBossBar UUID BossBarAction
  | CBServerDifficulty Difficulty
  | CBTabComplete (V.Vector T.Text)
  | CBChatMessage Chat Int8
  | CBMultiBlockChange Int32 Int32 (V.Vector BlockChange)
  | CBConfirmTransaction Int8 Short Bool
  | CBCloseWindow Word8
  | CBOpenWindow Word8 (Either Int32 T.Text) Chat Word8
  | CBWindowItems Word8 (V.Vector Slot)
  | CBWindowProperty Word8 Short Short
  | CBSetSlot Int8 Short Slot
  | CBSetCooldown VarInt VarInt
  | CBPluginMessage T.Text B.ByteString
  | CBNamedSoundEffect T.Text VarInt Int32 Int32 Int32 Float Float
  | CBPlayDisconnect Chat
  | CBEntityStatus Int32 EntityStatus
  | CBExplosion Float Float Float Float (V.Vector (Int8,Int8,Int8)) Float Float Float
  | CBUnloadChunk Int32 Int32
  | CBChangeGameState GameChangeReason Float
  | CBKeepAlive VarInt
  | CBChunkData ChunkColumn
  | CBEffect Int32 Position Int32 Bool
  | CBParticle Int32 Bool Float Float Float Float Float Float Float (V.Vector VarInt)
  | CBJoinGame Int32 GameMode Dimension Difficulty Word8 T.Text Bool
  | CBMap VarInt Int8 Bool (V.Vector Icon) UpdatedColumns
  | CBEntityRelativeMove VarInt Short Short Short Bool
  | CBEntityLookAndRelativeMove VarInt Short Short Short Angle Angle Bool
  | CBEntityLook VarInt Angle Angle Bool
  | CBEntity VarInt
  | CBVehicleMove Double Double Double Float Float
  | CBOpenSignEditor Position
  | CBPlayerAbilities Int8 Float Float
  | CBCombatEvent CombatEvent
  | CBPlayerListItem PlayerListEntries
  | CBPlayerPositionAndLook Double Double Double Float Float Int8 VarInt
  | CBUseBed VarInt Position
  | CBDestroyEntities (V.Vector VarInt)
  | CBRemoveEntityEffect VarInt Int8
  | CBResourcePackSend T.Text T.Text
  | CBRespawn Dimension Difficulty GameMode T.Text
  | CBEntityHeadLook VarInt Angle
  | CBWorldBorder WorldBorderAction
  | CBCamera VarInt
  | CBHeldItemChange Int8
  | CBDisplayScoreboard Int8 T.Text
  | CBEntityMetadata VarInt EntityMetadata
  | CBAttachEntity Int32 Int32
  | CBEntityVelocity VarInt Short Short Short
  | CBEntityEquipment VarInt VarInt Slot
  | CBSetExperience Float VarInt VarInt
  | CBUpdateHealth Float VarInt Float
  | CBScoreboardObjective T.Text ScoreboardMode
  | CBSetPassengers VarInt (V.Vector VarInt)
  | CBTeams T.Text TeamMode
  | CBUpdateScore UpdateScoreAction
  | CBSpawnPosition Position
  | CBTimeUpdate Int64 Int64
  | CBTitle TitleAction
  | CBSoundEffect VarInt VarInt Int32 Int32 Int32 Float Float
  | CBPlayerListHeaderAndFooter Chat Chat
  | CBCollectItem VarInt VarInt
  | CBEntityTeleport VarInt Double Double Double Angle Angle Bool
  | CBEntityProperties VarInt (V.Vector EntityProperty)
  | CBEntityEffect VarInt Int8 Int8 VarInt Word8
  deriving (Show,Eq)

instance Serialize CBPlay where
  put (CBSpawnObject entityID objectUUID entityType x y z pitch yaw dat vX vY vZ) = do
    putWord8 0x00
    putVarInt entityID
    putUUID objectUUID
    put entityType
    putFloat64be x
    putFloat64be y
    putFloat64be z
    putAngle pitch
    putAngle yaw
    put dat
    put vX
    put vY
    put vZ
  put (CBSpawnExperienceOrb entityID x y z count) = do
    putWord8 0x01
    putVarInt entityID
    putFloat64be x
    putFloat64be y
    putFloat64be z
    put count
  put (CBSpawnGlobalEntity entityID entityType x y z) = do
    putWord8 0x02
    putVarInt entityID
    put entityType
    putFloat64be x
    putFloat64be y
    putFloat64be z
  put (CBSpawnMob entityID entityUUID entityType x y z yaw pitch headPitch vX vY vZ metadata) = do
    putWord8 0x03
    putVarInt entityID
    putUUID entityUUID
    putWord8 entityType
    putFloat64be x
    putFloat64be y
    putFloat64be z
    putAngle yaw
    putAngle pitch
    putAngle headPitch
    put vX
    put vY
    put vZ
    putEntityMetadata metadata
  put (CBSpawnPainting entityID entityUUID title location direction) = do
    putWord8 0x04
    putVarInt entityID
    putUUID entityUUID
    putText title
    putPosition location
    put direction
  put (CBSpawnPlayer entityID playerUUID x y z yaw pitch metadata) = do
    putWord8 0x05
    putVarInt entityID
    putUUID playerUUID
    putFloat64be x
    putFloat64be y
    putFloat64be z
    putAngle yaw
    putAngle pitch
    putEntityMetadata metadata
  put (CBAnimation entityID animation) = do
    putWord8 0x06
    putVarInt entityID
    putWord8 . toEnum . fromEnum $ animation
  put (CBStatistics statistics) = do
    putWord8 0x07
    putVarInt . V.length $ statistics
    mapM_ put statistics
  put (CBBlockBreakAnimation entityID location destroyStage) = do
    putWord8 0x08
    putVarInt entityID
    putPosition location
    put destroyStage
  put (CBUpdateBlockEntity location action nbtData) = do
    putWord8 0x09
    putPosition location
    putWord8 . toEnum . fromEnum $ action
    put nbtData
  put (CBBlockAction location blockAction) = do
    putWord8 0x0A
    putPosition location
    put blockAction
  put (CBBlockChange location blockID) = do
    putWord8 0x0B
    putPosition location
    putVarInt blockID
  put (CBBossBar uuid bossBarAction) = do
    putWord8 0x0C
    putUUID uuid
    put bossBarAction
  put (CBServerDifficulty difficulty) = do
    putWord8 0x0D
    putWord8 . toEnum . fromEnum $ difficulty
  put (CBTabComplete matches) = do
    putWord8 0x0E
    putVarInt . V.length $ matches
    mapM_ putText matches
  put (CBChatMessage jsonData position) = do
    putWord8 0x0F
    putText jsonData
    put position
  put (CBMultiBlockChange chunkX chunkZ records) = do
    putWord8 0x10
    put chunkX
    put chunkZ
    putVarInt . V.length $ records
    mapM_ put records
  put (CBConfirmTransaction windowID actionNumber accepted) = do
    putWord8 0x11
    put windowID
    put actionNumber
    putWord8 . toEnum . fromEnum $ accepted
  put (CBCloseWindow windowID) = do
    putWord8 0x12
    putWord8 windowID
  put (CBOpenWindow windowID windowType windowTitle numOfSlots) = do
    putWord8 0x13
    putWord8 windowID
    case windowType of
      Left entityID -> do
        putText "EntityHorse"
        putText windowTitle
        putWord8 numOfSlots
        put entityID
      Right windowType' -> do
        putText windowType'
        putText windowTitle
        putWord8 numOfSlots
  put (CBWindowItems windowID slotData) = do
    putWord8 0x14
    putWord8 windowID
    put (toEnum . V.length $ slotData :: Int16)
    mapM_ put slotData
  put (CBWindowProperty windowID property value) = do
    putWord8 0x15
    putWord8 windowID
    put property -- Should move to WindowProperty
    put value
  put (CBSetSlot windowID slot slotData) = do
    putWord8 0x16
    put windowID
    put slot
    put slotData
  put (CBSetCooldown itemID cooldownTicks) = do
    putWord8 0x17
    putVarInt itemID
    putVarInt cooldownTicks
  put (CBPluginMessage channel dat) = do
    putWord8 0x18
    putText channel
    putByteString dat
  put (CBNamedSoundEffect soundName soundCategory effPosX effPosY effPosZ volume pitch) = do
    putWord8 0x19
    putText soundName
    putVarInt soundCategory
    put effPosX
    put effPosY
    put effPosZ
    putFloat32be volume
    putFloat32be pitch
  put (CBPlayDisconnect reason) = do
    putWord8 0x1A
    putText reason
  put (CBEntityStatus entityID entityStatus) = do
    putWord8 0x1B
    put entityID
    put (toEnum . fromEnum $ entityStatus :: Int8)
  put (CBExplosion x y z radius records pMotionX pMotionY pMotionZ) = do
    putWord8 0x1C
    putFloat32be x
    putFloat32be y
    putFloat32be z
    putFloat32be radius
    put (toEnum . V.length $ records :: Int32)
    mapM_ (\(a,b,c) -> do { put a; put b; put c}) records
    putFloat32be pMotionX
    putFloat32be pMotionY
    putFloat32be pMotionZ
  put (CBUnloadChunk chunkX chunkZ) = do
    putWord8 0x1D
    put chunkX
    put chunkZ
  put (CBChangeGameState reason value) = do
   putWord8 0x1E
   put reason
   putFloat32be value
  put (CBKeepAlive keepAliveID) = do
    putWord8 0x1F
    putVarInt keepAliveID
  put (CBChunkData chunkColumnData) = do
    putWord8 0x20
    put chunkColumnData
  -- (NOTE) Should be better typed to Effect IDs that actually exist
  put (CBEffect effectID location dat disableRelativeVolume) = do
    putWord8 0x21
    put effectID
    putPosition location
    put dat
    put disableRelativeVolume
  put (CBParticle particleID longDistance x y z offsetX offsetY offsetZ particleData dat) = do
    putWord8 0x22
    put particleID
    put longDistance
    putFloat32be x
    putFloat32be y
    putFloat32be z
    putFloat32be offsetX
    putFloat32be offsetY
    putFloat32be offsetZ
    putFloat32be particleData
    put (toEnum . V.length $ dat :: Int32)
    mapM_ putVarInt dat
  put (CBJoinGame entityID gameMode dimension difficulty maxPlayers levelType reduceDebug) = do
    putWord8 0x23
    put entityID
    put gameMode
    put dimension
    put difficulty
    putWord8 maxPlayers
    putText levelType
    put reduceDebug
  put (CBMap itemDamage scale trackingPosition icons updatedColumns) = do
    putWord8 0x24
    putVarInt itemDamage
    put scale
    put trackingPosition
    putVarInt . V.length $ icons
    mapM_ put icons
    put updatedColumns
  put (CBEntityRelativeMove entityID dX dY dZ onGround) = do
    putWord8 0x25
    putVarInt entityID
    put dX
    put dY
    put dZ
    put onGround
  put (CBEntityLookAndRelativeMove entityID dX dY dZ yaw pitch onGround) = do
    putWord8 0x26
    putVarInt entityID
    put dX
    put dY
    put dZ
    putAngle yaw
    putAngle pitch
    put onGround
  put (CBEntityLook entityID yaw pitch onGround) = do
    putWord8 0x27
    putVarInt entityID
    putAngle yaw
    putAngle pitch
    put onGround
  put (CBEntity entityID) = do
    putWord8 0x28
    putVarInt entityID
  put (CBVehicleMove x y z yaw pitch) = do
    putWord8 0x29
    putFloat64be x
    putFloat64be y
    putFloat64be z
    putFloat32be yaw
    putFloat32be pitch
  put (CBOpenSignEditor location) = do
    putWord8 0x2A
    putPosition location
  put (CBPlayerAbilities flags flyingSpeed viewModifiers) = do
    putWord8 0x2B
    put flags
    putFloat32be flyingSpeed
    putFloat32be viewModifiers
  put (CBCombatEvent combatEvent) = do
    putWord8 0x2C
    put combatEvent
  put (CBPlayerListItem entries) = do
    putWord8 0x2D
    put entries
  -- flags should be better typed
  put (CBPlayerPositionAndLook x y z yaw pitch flags teleportID) = do
    putWord8 0x2E
    putFloat64be x
    putFloat64be y
    putFloat64be z
    putFloat32be yaw
    putFloat32be pitch
    put flags
    putVarInt teleportID
  put (CBUseBed entityID location) = do
    putWord8 0x2F
    putVarInt entityID
    putPosition location
  put (CBDestroyEntities entityIDs) = do
    putWord8 0x30
    putVarInt . V.length $ entityIDs
    mapM_ putVarInt entityIDs
  put (CBRemoveEntityEffect entityID effectID) = do
    putWord8 0x31
    putVarInt entityID
    put effectID
  put (CBResourcePackSend url hash) = do
    putWord8 0x32
    putText url
    putText hash
  put (CBRespawn dimension difficulty gameMode levelType) = do
    putWord8 0x33
    put dimension
    put difficulty
    put gameMode
    putText levelType
  put (CBEntityHeadLook entityID headYaw) = do
    putWord8 0x34
    putVarInt entityID
    putAngle headYaw
  put (CBWorldBorder worldBorderAction) = do
    putWord8 0x35
    put worldBorderAction
  put (CBCamera cameraID) = do
    putWord8 0x36
    putVarInt cameraID
  put (CBHeldItemChange slot) = do
    putWord8 0x37
    put slot
  put (CBDisplayScoreboard position scoreName) = do
    putWord8 0x38
    put position
    putText scoreName
  put (CBEntityMetadata entityID metadata) = do
    putWord8 0x39
    putVarInt entityID
    putEntityMetadata metadata
  put (CBAttachEntity attachedEntityID holdingEntityID) = do
    putWord8 0x3A
    put attachedEntityID
    put holdingEntityID
  put (CBEntityVelocity entityID vX vY vZ) = do
    putWord8 0x3B
    putVarInt entityID
    put vX
    put vY
    put vZ
  put (CBEntityEquipment entityID slot item) = do
    putWord8 0x3C
    putVarInt entityID
    putVarInt slot
    put item
  put (CBSetExperience experienceBar level totalExperience) = do
    putWord8 0x3D
    putFloat32be experienceBar
    putVarInt level
    putVarInt totalExperience
  put (CBUpdateHealth health food foodSaturation) = do
    putWord8 0x3E
    putFloat32be health
    putVarInt food
    putFloat32be foodSaturation
  put (CBScoreboardObjective objectiveName mode) = do
    putWord8 0x3F
    putText objectiveName
    put mode
  put (CBSetPassengers entityID passengers) = do
    putWord8 0x40
    putVarInt entityID
    putVarInt . V.length $ passengers
    mapM_ putVarInt passengers
  put (CBTeams teamName mode) = do
    putWord8 0x41
    putText teamName
    put mode
  put (CBUpdateScore action) = do
    putWord8 0x42
    put action
  put (CBSpawnPosition location) = do
    putWord8 0x43
    putPosition location
  put (CBTimeUpdate worldAge timeOfDay) = do
    putWord8 0x44
    put worldAge
    put timeOfDay
  put (CBTitle titleAction) = do
    putWord8 0x45
    put titleAction
  put (CBSoundEffect soundID soundCategory effPosX effPosY effPosZ volume pitch) = do
    putWord8 0x46
    putVarInt soundID
    putVarInt soundCategory
    put effPosX
    put effPosY
    put effPosZ
    put volume
    put pitch
  put (CBPlayerListHeaderAndFooter header footer) = do
    putWord8 0x47
    putText header
    putText footer
  put (CBCollectItem collectedEntityID collectorEntityID) = do
    putWord8 0x48
    putVarInt collectedEntityID
    putVarInt collectorEntityID
  put (CBEntityTeleport entityID x y z yaw pitch onGround) = do
    putWord8 0x49
    putVarInt entityID
    putFloat64be x
    putFloat64be y
    putFloat64be z
    putAngle yaw
    putAngle pitch
    put onGround
  -- Needs to be better typed
  put (CBEntityProperties entityID properties) = do
    putWord8 0x4A
    putVarInt entityID
    put (toEnum . V.length $ properties :: Int32)
    mapM_ put properties
  put (CBEntityEffect entityID effectID amplifier duration hideParticles) = do
    putWord8 0x4B
    putVarInt entityID
    put effectID
    put amplifier
    putVarInt duration
    put hideParticles

  get = do
    packetID <- getWord8
    case packetID of
      0x00 -> do
        entityID <- getVarInt
        objectUUID <- getUUID
        t <- getInt8
        x <- getFloat64be
        y <- getFloat64be
        z <- getFloat64be
        pitch <- getAngle
        yaw <- getAngle
        dat <- getInt32be
        vX <- getInt16be
        vY <- getInt16be
        vZ <- getInt16be
        return $ CBSpawnObject entityID objectUUID t x y z pitch yaw dat vX vY vZ
      0x01 -> CBSpawnExperienceOrb <$> getVarInt <*> getFloat64be <*> getFloat64be <*> getFloat64be <*> getInt16be
      0x02 -> CBSpawnGlobalEntity <$> getVarInt <*> getInt8 <*> getFloat64be <*> getFloat64be <*> getFloat64be
      0x03 -> do
        entityID <- getVarInt
        entityUUID <- getUUID
        t <- getWord8
        x <- getFloat64be
        y <- getFloat64be
        z <- getFloat64be
        yaw <- getAngle
        pitch <- getAngle
        headPitch <- getAngle
        vX <- getInt16be
        vY <- getInt16be
        vZ <- getInt16be
        metadata <- getEntityMetadata
        return $ CBSpawnMob entityID entityUUID t x y z yaw pitch headPitch vX vY vZ metadata
      0x04 -> do
        entityID <- getVarInt
        entityUUID <- getUUID
        title <- getText
        location <- getPosition
        direction <- getInt8
        return $ CBSpawnPainting entityID entityUUID title location direction
      0x05 -> do
        entityID <- getVarInt
        playerUUID <- getUUID
        x <- getFloat64be
        y <- getFloat64be
        z <- getFloat64be
        yaw <- getAngle
        pitch <- getAngle
        metadata <- getEntityMetadata
        return $ CBSpawnPlayer entityID playerUUID x y z yaw pitch metadata
      0x06 -> CBAnimation <$> getVarInt <*> get
      0x07 -> CBStatistics <$> (getVarInt >>= \n -> V.replicateM (fromEnum n) get)
      0x08 -> CBBlockBreakAnimation <$> getVarInt <*> getPosition <*> getInt8
      0x09 -> CBUpdateBlockEntity <$> getPosition <*> get <*> get
      0x0A -> CBBlockAction <$> getPosition <*> get
      0x0B -> CBBlockChange <$> getPosition <*> getVarInt
      0x0C -> CBBossBar <$> getUUID <*> get
      0x0D -> CBServerDifficulty <$> get
      0x0E -> CBTabComplete <$> (getVarInt >>= \n -> V.replicateM n getText)
      0x0F -> CBChatMessage <$> getText <*> getInt8
      0x10 -> CBMultiBlockChange <$> getInt32be <*> getInt32be <*> (getVarInt >>= \n -> V.replicateM n get)
      0x11 -> CBConfirmTransaction <$> getInt8 <*> getInt16be <*> get
      0x12 -> CBCloseWindow <$> getWord8
      0x13 -> do
        windowID <- getWord8
        windowType <- getText
        windowTitle <- getText
        numberOfSlots <- getWord8
        case windowType of
          "EntityHorse" -> do
            entityID <- getInt32be
            return $ CBOpenWindow windowID (Left entityID) windowTitle numberOfSlots
          _ -> return $ CBOpenWindow windowID (Right windowType) windowTitle numberOfSlots
      0x14 -> do
        windowID <- getWord8
        count <- fmap fromEnum getInt16be
        slotData <- V.replicateM count get
        return $ CBWindowItems windowID slotData
      0x15 -> CBWindowProperty <$> getWord8 <*> getInt16be <*> getInt16be
      0x16 -> CBSetSlot <$> getInt8 <*> getInt16be <*> get
      0x17 -> CBSetCooldown <$> getVarInt <*> getVarInt
      0x18 -> CBPluginMessage <$> getText <*> (remaining >>= \n -> getByteString n)
      0x19 -> do
        soundName <- getText
        soundCategory <- getVarInt
        effectPosX <- getInt32be
        effectPosY <- getInt32be
        effectPosZ <- getInt32be
        volume <- getFloat32be
        pitch <- getFloat32be
        return $ CBNamedSoundEffect soundName soundCategory effectPosX effectPosY effectPosZ volume pitch
      0x1A -> CBPlayDisconnect <$> getText
      0x1B -> CBEntityStatus <$> getInt32be <*> get
      0x1C -> do
        x <- getFloat32be
        y <- getFloat32be
        z <- getFloat32be
        radius <- getFloat32be
        count <- fmap fromEnum getInt32be
        records <- V.replicateM count (do a <- getInt8
                                          b <- getInt8
                                          c <- getInt8
                                          return (a,b,c))
        pMotionX <- getFloat32be
        pMotionY <- getFloat32be
        pMotionZ <- getFloat32be
        return $ CBExplosion x y z radius records pMotionX pMotionY pMotionZ
      0x1D -> CBUnloadChunk <$> getInt32be <*> getInt32be
      0x1E -> CBChangeGameState <$> get <*> getFloat32be
      0x1F -> CBKeepAlive <$> getVarInt
      0x20 -> CBChunkData <$> get
      0x21 -> CBEffect <$> getInt32be <*> getPosition <*> getInt32be <*> get
      0x22 -> do
        particleID <- getInt32be
        longDistance <- get
        x <- getFloat32be
        y <- getFloat32be
        z <- getFloat32be
        offsetX <- getFloat32be
        offsetY <- getFloat32be
        offsetZ <- getFloat32be
        particleData <- getFloat32be
        particleCount <- getInt32be
        dat <- V.replicateM (fromEnum particleCount) getVarInt
        return $ CBParticle particleID longDistance x y z offsetX offsetY offsetZ particleData dat
      0x23 -> do
        entityID <- getInt32be
        gameMode <- get
        dimension <- get
        difficulty <- get
        maxPlayers <- getWord8
        levelType <- getText
        reducedDebugInfo <- get
        return $ CBJoinGame entityID gameMode dimension difficulty maxPlayers levelType reducedDebugInfo
      0x24 -> do
        itemDamage <- getVarInt
        scale <- getInt8
        trackingPositon <- get
        count <- getVarInt
        icons <- V.replicateM count get
        updateColumns <- get
        return $ CBMap itemDamage scale trackingPositon icons updateColumns
      0x25 -> CBEntityRelativeMove <$> getVarInt <*> getInt16be <*> getInt16be <*> getInt16be <*> get
      0x26 -> do
        entityID <- getVarInt
        dX <- getInt16be
        dY <- getInt16be
        dZ <- getInt16be
        yaw <- getAngle
        pitch <- getAngle
        onGround <- get
        return $ CBEntityLookAndRelativeMove entityID dX dY dZ yaw pitch onGround
      0x27 -> CBEntityLook <$> getVarInt <*> getAngle <*> getAngle <*> get
      0x28 -> CBEntity <$> getVarInt
      0x29 -> CBVehicleMove <$> getFloat64be <*> getFloat64be <*> getFloat64be <*> getFloat32be <*> getFloat32be
      0x2A -> CBOpenSignEditor <$> getPosition
      0x2B -> CBPlayerAbilities <$> getInt8 <*> getFloat32be <*> getFloat32be
      0x2C -> CBCombatEvent <$> get
      0x2D -> CBPlayerListItem <$> get
      0x2E -> do
        x <- getFloat64be
        y <- getFloat64be
        z <- getFloat64be
        yaw <- getFloat32be
        pitch <- getFloat32be
        flags <- getInt8
        teleportID <- getVarInt
        return $ CBPlayerPositionAndLook x y z yaw pitch flags teleportID
      0x2F -> CBUseBed <$> getVarInt <*> getPosition
      0x30 -> CBDestroyEntities <$> (getVarInt >>= \n -> V.replicateM n getVarInt)
      0x31 -> CBRemoveEntityEffect <$> getVarInt <*> getInt8
      0x32 -> CBResourcePackSend <$> getText <*> getText
      0x33 -> CBRespawn <$> get <*> get <*> get <*> getText
      0x34 -> CBEntityHeadLook <$> getVarInt <*> getAngle
      0x35 -> CBWorldBorder <$> get
      0x36 -> CBCamera <$> getVarInt
      0x37 -> CBHeldItemChange <$> getInt8
      0x38 -> CBDisplayScoreboard <$> getInt8 <*> getText
      0x39 -> CBEntityMetadata <$> getVarInt <*> getEntityMetadata
      0x3A -> CBAttachEntity <$> getInt32be <*> getInt32be
      0x3B -> CBEntityVelocity <$> getVarInt <*> getInt16be <*> getInt16be <*> getInt16be
      0x3C -> CBEntityEquipment <$> getVarInt <*> getVarInt <*> get
      0x3D -> CBSetExperience <$> getFloat32be <*> getVarInt <*> getVarInt
      0x3E -> CBUpdateHealth <$> getFloat32be <*> getVarInt <*> getFloat32be
      0x3F -> CBScoreboardObjective <$> getText <*> get
      0x40 -> CBSetPassengers <$> getVarInt <*> (getVarInt >>= \n -> V.replicateM n getVarInt)
      0x41 -> CBTeams <$> getText <*> get
      0x42 -> CBUpdateScore <$> get
      0x43 -> CBSpawnPosition <$> getPosition
      0x44 -> CBTimeUpdate <$> getInt64be <*> getInt64be
      0x45 -> CBTitle <$> get
      0x46 -> CBSoundEffect <$> getVarInt <*> getVarInt <*> getInt32be <*> getInt32be <*> getInt32be <*> getFloat32be <*> getFloat32be
      0x47 -> CBPlayerListHeaderAndFooter <$> getText <*> getText
      0x48 -> CBCollectItem <$> getVarInt <*> getVarInt
      0x49 -> CBEntityTeleport <$> getVarInt <*> getFloat64be <*> getFloat64be <*> getFloat64be <*> getAngle <*> getAngle <*> get
      0x4A -> CBEntityProperties <$> getVarInt <*> (getInt32be >>= \n -> V.replicateM (fromEnum n) get)
      0x4B -> CBEntityEffect <$> getVarInt <*> getInt8 <*> getInt8 <*> getVarInt <*> get
      err -> fail $ "Unrecognized packetID: " ++ show err

data SBPlay
  = SBTeleportConfirm VarInt
  | SBTabComplete T.Text Bool (Maybe Position)
  | SBChatMessage T.Text
  | SBClientStatus VarInt
  | SBClientSettings T.Text Int8 VarInt Bool Word8 VarInt
  | SBConfirmTransaction Int8 Short Bool
  | SBEnchantItem Int8 Int8
  | SBClickWindow Word8 Short Int8 Short VarInt Slot
  | SBCloseWindow Word8
  | SBPluginMessage T.Text B.ByteString
  | SBUseEntity VarInt UseEntityType
  | SBKeepAlive VarInt
  | SBPlayerPosition Double Double Double Bool
  | SBPlayerPositionAndLook Double Double Double Float Float Bool
  | SBPlayerLook Float Float Bool
  | SBPlayer Bool
  | SBVehicleMove Double Double Double Float Float
  | SBSteerBoat Bool Bool
  | SBPlayerAbilities Int8 Float Float
  | SBPlayerDigging VarInt Position Int8
  | SBEntityAction VarInt VarInt VarInt
  | SBSteerVehicle Float Float Word8
  | SBResourcePackStatus VarInt
  | SBHeldItemChange Short
  | SBCreativeInventoryAction Short Slot
  | SBUpdateSign Position T.Text T.Text T.Text T.Text
  | SBAnimation VarInt
  | SBSpectate UUID
  | SBPlayerBlockPlacement Position VarInt VarInt Word8 Word8 Word8
  | SBUseItem VarInt
  deriving (Show,Eq)

instance Serialize SBPlay where
  put (SBTeleportConfirm teleportID) = do
    putWord8 0x00
    putVarInt teleportID
  put (SBTabComplete text assumeCommand lookedAtBlock) = do
    putWord8 0x01
    putText text
    put assumeCommand
    case lookedAtBlock of
        Just lookedAtBlock' -> do
          put True
          putPosition lookedAtBlock'
        Nothing -> do
          put False
  put (SBChatMessage message) = do
    putWord8 0x02
    putText message
  put (SBClientStatus actionID) = do
    putWord8 0x03
    putVarInt actionID
  put (SBClientSettings locale viewDistance chatMode chatColors displayedSkinParts mainHand) = do
    putWord8 0x04
    putText locale
    put viewDistance
    putVarInt chatMode
    (putWord8 . toEnum . fromEnum $ chatColors)
    putWord8 displayedSkinParts
    putVarInt mainHand
  put (SBConfirmTransaction windowID actionNumber accepted) = do
    putWord8 0x05
    put windowID
    put actionNumber
    put accepted
  put (SBEnchantItem windowID enchantment) = do
    putWord8 0x06
    put windowID
    put enchantment
  put (SBClickWindow windowID slot button actionNumber mode clickedItem) = do
    putWord8 0x07
    putWord8 windowID
    put slot
    put button
    put actionNumber
    putVarInt mode
    put clickedItem
  put (SBCloseWindow windowID) = do
    putWord8 0x08
    putWord8 windowID
  put (SBPluginMessage channel dat) = do
    putWord8 0x09
    putText channel
    putNetcodeByteString dat
  put (SBUseEntity target t) = do
    putWord8 0x0A
    putVarInt target
    put t
  put (SBKeepAlive keepAliveID) = do
    putWord8 0x0B
    putVarInt keepAliveID
  put (SBPlayerPosition x feetY z onGround) = do
    putWord8 0x0C
    putFloat64be x
    putFloat64be feetY
    putFloat64be z
    put onGround
  put (SBPlayerPositionAndLook x feetY z yaw pitch onGround) = do
    putWord8 0x0D
    putFloat64be x
    putFloat64be feetY
    putFloat64be z
    putFloat32be yaw
    putFloat32be pitch
    put onGround
  put (SBPlayerLook yaw pitch onGround) = do
    putWord8 0x0E
    putFloat32be yaw
    putFloat32be pitch
    put onGround
  put (SBPlayer onGround) = do
    putWord8 0x0F
    put onGround
  put (SBVehicleMove x y z yaw pitch) = do
    putWord8 0x10
    putFloat64be x
    putFloat64be y
    putFloat64be z
    putFloat32be yaw
    putFloat32be pitch
  put (SBSteerBoat rightPaddle leftPaddle) = do
    putWord8 0x11
    put rightPaddle
    put leftPaddle
  put (SBPlayerAbilities flags flyingSpeed walkingSpeed) = do
    putWord8 0x12
    put flags
    putFloat32be flyingSpeed
    putFloat32be walkingSpeed
  put (SBPlayerDigging status location face) = do
    putWord8 0x13
    putVarInt status
    putPosition location
    put face
  put (SBEntityAction entityID actionID jumpBoost) = do
    putWord8 0x14
    putVarInt entityID
    putVarInt actionID
    putVarInt jumpBoost
  put (SBSteerVehicle sideways forward flags) = do
    putWord8 0x15
    putFloat32be sideways
    putFloat32be forward
    putWord8 flags
  put (SBResourcePackStatus result) = do
    putWord8 0x16
    putVarInt result
  put (SBHeldItemChange slot) = do
    putWord8 0x17
    put slot
  put (SBCreativeInventoryAction slot clickedItem) = do
    putWord8 0x18
    put slot
    put clickedItem
  put (SBUpdateSign location line1 line2 line3 line4) = do
    putWord8 0x19
    putPosition location
    putText line1
    putText line2
    putText line3
    putText line4
  put (SBAnimation hand) = do
    putWord8 0x1A
    putVarInt hand
  put (SBSpectate targetPlayer) = do
    putWord8 0x1B
    putUUID targetPlayer
  put (SBPlayerBlockPlacement location face hand cursorPosX cursorPosY cursorPosZ) = do
    putWord8 0x1C
    putPosition location
    putVarInt face
    putVarInt hand
    putWord8 cursorPosX
    putWord8 cursorPosY
    putWord8 cursorPosZ
  put (SBUseItem hand) = do
    putWord8 0x1D
    putVarInt hand

  get = do
    packetID <- getWord8
    case packetID of
      0x00  -> SBTeleportConfirm <$> getVarInt
      0x01 -> do
        text <- getText
        assumeCommand <- get
        hasPosition <- get
        if hasPosition
          then do
            lookedAtBlock <- getPosition
            return $
              SBTabComplete
                text
                assumeCommand
                (Just lookedAtBlock)
          else return $
               SBTabComplete
                text
                assumeCommand
                Nothing
      0x02 -> SBChatMessage <$> getText
      0x03 -> SBClientStatus <$> getVarInt
      0x04 -> SBClientSettings <$> getText <*> getInt8 <*> getVarInt <*> get <*> getWord8 <*> getVarInt
      0x05 -> SBConfirmTransaction <$> getInt8 <*> getInt16be <*> get
      0x06 -> SBEnchantItem <$> getInt8 <*> getInt8
      0x07 -> SBClickWindow <$> getWord8 <*> getInt16be <*> getInt8 <*> getInt16be <*> getVarInt <*> get
      0x08 -> SBCloseWindow <$> getWord8
      0x09 -> SBPluginMessage <$> getText <*> (getVarInt >>= \n -> getByteString n)
      0x0A -> SBUseEntity <$> getVarInt <*> get
      0x0B -> SBKeepAlive <$> getVarInt
      0x0C -> SBPlayerPosition <$> getFloat64be <*> getFloat64be <*> getFloat64be <*> get
      0x0D -> SBPlayerPositionAndLook <$> getFloat64be <*> getFloat64be <*> getFloat64be <*> getFloat32be <*> getFloat32be <*> get
      0x0E -> SBPlayerLook <$> getFloat32be <*> getFloat32be <*> get
      0x0F -> SBPlayer <$> get
      0x10 -> SBVehicleMove <$> getFloat64be <*> getFloat64be <*> getFloat64be <*> getFloat32be <*> getFloat32be
      0x11 -> SBSteerBoat <$> get <*> get
      0x12 -> SBPlayerAbilities <$> getInt8 <*> getFloat32be <*> getFloat32be
      0x13 -> SBPlayerDigging <$> getVarInt <*> getPosition <*> getInt8
      0x14 -> SBEntityAction <$> getVarInt <*> getVarInt <*> getVarInt
      0x15 -> SBSteerVehicle <$> getFloat32be <*> getFloat32be <*> getWord8
      0x16 -> SBResourcePackStatus <$> getVarInt
      0x17 -> SBHeldItemChange <$> getInt16be
      0x18 -> SBCreativeInventoryAction <$> getInt16be <*> get
      0x19 -> SBUpdateSign <$> getPosition <*> getText <*> getText <*> getText <*> getText
      0x1A -> SBAnimation <$> getVarInt
      0x1B -> SBSpectate <$> getUUID
      0x1C -> SBPlayerBlockPlacement <$> getPosition <*> getVarInt <*> getVarInt <*> getWord8 <*> getWord8 <*> getWord8
      0x1D -> SBUseItem <$> getVarInt
      err -> fail $ "Unrecognized packetID: " ++ show err
