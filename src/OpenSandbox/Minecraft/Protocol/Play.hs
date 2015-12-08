-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.Protocol.Play
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Protocol.Play
    ( ClientBoundPlay
    , ServerBoundPlay
    ) where

import qualified  Data.ByteString as B
import            Data.Serialize
import            Data.Serialize.Get
import            Data.Serialize.Put
import            Data.Word

type VarInt = Word8


type Position = Word64


data ClientBoundPlay
  = ClientBoundKeepAlive
  | ClientBoundLogin
  | ClientBoundChat
  | ClientBoundUpdateTime
  | ClientBoundEntityEquipment
  | ClientBoundSpawnPosition
  | ClientBoundUpdateHealth
  | ClientBoundRespawn
  | ClientBoundPosition
  | ClientBoundHeldItemSlot
  | ClientBoundBed
  | ClientBoundAnimation
  | ClientBoundNamedEntitySpawn
  | ClientBoundCollect
  | ClientBoundSpawnEntity
  | ClientBoundSpawnEntityLiving
  | ClientBoundSpawnEntityPainting
  | ClientBoundSpawnEntityExperienceOrb
  | ClientBoundEntityVelocity
  | ClientBoundEntityDestroy
  | ClientBoundEntity
  | ClientBoundRelEntityMove
  | ClientBoundEntityLook
  | ClientBoundEntityMoveLook
  | ClientBoundEntityTeleport
  | ClientBoundEntityHeadRotation
  | ClientBoundEntityStatus
  | ClientBoundAttachEntity
  | ClientBoundEntityMetadata
  | ClientBoundEntityEffect
  | ClientBoundRemoveEntityEffect
  | ClientBoundExperience
  | ClientBoundUpdateAttributes
  | ClientBoundMapChunk
  | ClientBoundMultiBlockChange
  | ClientBoundBlockChange
  | ClientBoundBlockAction
  | ClientBoundBlockBreakAnimation
  | ClientBoundExplosion
  | ClientBoundWorldEvent
  | ClientBoundNamedSoundEffect
  | ClientBoundWorldParticles
  | ClientBoundGameStateChange
  | ClientBoundSpawnEntityWeather
  | ClientBoundOpenWindow
  | ClientBoundCloseWindow
  | ClientBoundSetSlot
  | ClientBoundWindowItems
  | ClientBoundCraftProgressBar
  | ClientBoundTransaction
  | ClientBoundUpdateSign
  | ClientBoundMap
  | ClientBoundTileEntityData
  | ClientBoundOpenSignEntity
  | ClientBoundStatistics
  | ClientBoundPlayerInfo
  | ClientBoundAbilities
  | ClientBoundTabComplete
  | ClientBoundScoreBoardObjective
  | ClientBoundScoreBoardScore
  | ClientBoundScoreBoardDisplayObjective
  | ClientBoundScoreBoardTeam
  | ClientBoundCustomPayload
  | ClientBoundKickDisconnect
  | ClientBoundDifficulty
  | ClientBoundCombatEvent
  | ClientBoundCamera
  | ClientBoundWorldBorder
  | ClientBoundTitle
  | ClientBoundPlaySetCompression
  | ClientBoundPlayerlistHeader
  | ClientBoundResourcePackSend
  | ClientBoundBossBar
  | ClientBoundSetCooldown
  | ClientBoundUnloadChunk
  deriving (Show,Eq)


data ServerBoundPlay
  = ServerBoundKeepAlive
  | ServerBoundChat
  | ServerBoundUseEntity
  | ServerBoundFlying
  | ServerBoundPosition
  | ServerBoundLook
  | ServerBoundPositionLook
  | ServerBoundBlockDig
  | ServerBoundBlockPlace
  | ServerBoundHeldItemSlot
  | ServerBoundArmAnimation
  | ServerBoundEntityAction
  | ServerBoundSteerVehicle
  | ServerBoundCloseWindow
  | ServerBoundWindowClick
  | ServerBoundTransaction
  | ServerBoundSetCreativeSlot
  | ServerBoundEnchantItem
  | ServerBoundUpdateSign
  | ServerBoundAbilities
  | ServerBoundTabComplete
  | ServerBoundSettings
  | ServerBoundClientCommand
  | ServerBoundCustomPayload
  | ServerBoundSpectate
  | ServerBoundResourcePackReceive
  | ServerBoundUseItem
  deriving (Show,Eq)


instance Serialize ClientBoundPlay where
  put ClientBoundKeepAlive = undefined
  put ClientBoundLogin = undefined
  put ClientBoundChat = undefined
  put ClientBoundUpdateTime = undefined
  put ClientBoundEntityEquipment = undefined
  put ClientBoundSpawnPosition = undefined
  put ClientBoundUpdateHealth = undefined
  put ClientBoundRespawn = undefined
  put ClientBoundPosition = undefined
  put ClientBoundHeldItemSlot = undefined
  put ClientBoundBed = undefined
  put ClientBoundAnimation = undefined
  put ClientBoundNamedEntitySpawn = undefined
  put ClientBoundCollect = undefined
  put ClientBoundSpawnEntity = undefined
  put ClientBoundSpawnEntityLiving = undefined
  put ClientBoundSpawnEntityPainting = undefined
  put ClientBoundSpawnEntityExperienceOrb = undefined
  put ClientBoundEntityVelocity = undefined
  put ClientBoundEntityDestroy = undefined
  put ClientBoundEntity = undefined
  put ClientBoundRelEntityMove = undefined
  put ClientBoundEntityLook = undefined
  put ClientBoundEntityMoveLook = undefined
  put ClientBoundEntityTeleport = undefined
  put ClientBoundEntityHeadRotation = undefined
  put ClientBoundEntityStatus = undefined
  put ClientBoundAttachEntity = undefined
  put ClientBoundEntityMetadata = undefined
  put ClientBoundEntityEffect = undefined
  put ClientBoundRemoveEntityEffect = undefined
  put ClientBoundExperience = undefined
  put ClientBoundUpdateAttributes = undefined
  put ClientBoundMapChunk = undefined
  put ClientBoundMultiBlockChange = undefined
  put ClientBoundBlockChange = undefined
  put ClientBoundBlockAction = undefined
  put ClientBoundBlockBreakAnimation = undefined
  put ClientBoundExplosion = undefined
  put ClientBoundWorldEvent = undefined
  put ClientBoundNamedSoundEffect = undefined
  put ClientBoundWorldParticles = undefined
  put ClientBoundGameStateChange = undefined
  put ClientBoundSpawnEntityWeather = undefined
  put ClientBoundOpenWindow = undefined
  put ClientBoundCloseWindow = undefined
  put ClientBoundSetSlot = undefined
  put ClientBoundWindowItems = undefined
  put ClientBoundCraftProgressBar = undefined
  put ClientBoundTransaction = undefined
  put ClientBoundUpdateSign = undefined
  put ClientBoundMap = undefined
  put ClientBoundTileEntityData = undefined
  put ClientBoundOpenSignEntity = undefined
  put ClientBoundStatistics = undefined
  put ClientBoundPlayerInfo = undefined
  put ClientBoundAbilities = undefined
  put ClientBoundTabComplete = undefined
  put ClientBoundScoreBoardObjective = undefined
  put ClientBoundScoreBoardScore = undefined
  put ClientBoundScoreBoardDisplayObjective = undefined
  put ClientBoundScoreBoardTeam = undefined
  put ClientBoundCustomPayload = undefined
  put ClientBoundKickDisconnect = undefined
  put ClientBoundDifficulty = undefined
  put ClientBoundCombatEvent = undefined
  put ClientBoundCamera = undefined
  put ClientBoundWorldBorder = undefined
  put ClientBoundTitle = undefined
  put ClientBoundPlaySetCompression = undefined
  put ClientBoundPlayerlistHeader = undefined
  put ClientBoundResourcePackSend = undefined
  put ClientBoundBossBar = undefined
  put ClientBoundSetCooldown = undefined
  put ClientBoundUnloadChunk = undefined

  get = do
    _ <- getWord8
    packetID <- getWord8
    case packetID of
      0x00 -> return ClientBoundSpawnEntity
      0x01 -> return ClientBoundSpawnEntityExperienceOrb
      0x02 -> return ClientBoundSpawnEntityWeather
      0x03 -> return ClientBoundSpawnEntityLiving
      0x04 -> return ClientBoundSpawnEntityPainting
      0x05 -> return ClientBoundNamedEntitySpawn
      0x06 -> return ClientBoundAnimation
      0x07 -> return ClientBoundStatistics
      0x08 -> return ClientBoundBlockBreakAnimation
      0x09 -> return ClientBoundTileEntityData
      0x0c -> return ClientBoundBossBar
      0x0d -> return ClientBoundDifficulty
      0x0e -> return ClientBoundTabComplete
      0x11 -> return ClientBoundTransaction
      0x12 -> return ClientBoundCloseWindow
      0x13 -> return ClientBoundOpenWindow
      0x14 -> return ClientBoundWindowItems
      0x15 -> return ClientBoundCraftProgressBar
      0x16 -> return ClientBoundSetSlot
      0x17 -> return ClientBoundSetCooldown
      0x18 -> return ClientBoundCustomPayload
      0x19 -> return ClientBoundKickDisconnect
      0x0a -> return ClientBoundBlockAction
      0x0b -> return ClientBoundBlockChange
      0x0f -> return ClientBoundChat
      0x10 -> return ClientBoundMultiBlockChange
      0x1a -> return ClientBoundEntityStatus
      0x1b -> return ClientBoundExplosion
      0x1c -> return ClientBoundUnloadChunk
      0x1d -> return ClientBoundPlaySetCompression
      0x1e -> return ClientBoundGameStateChange
      0x1f -> return ClientBoundKeepAlive
      0x20 -> return ClientBoundMapChunk
      0x21 -> return ClientBoundWorldEvent
      0x22 -> return ClientBoundWorldParticles
      0x23 -> return ClientBoundNamedSoundEffect
      0x24 -> return ClientBoundLogin
      0x25 -> return ClientBoundMap
      0x26 -> return ClientBoundRelEntityMove
      0x27 -> return ClientBoundEntityMoveLook
      0x28 -> return ClientBoundEntityLook
      0x29 -> return ClientBoundEntity
      0x2a -> return ClientBoundOpenSignEntity
      0x2b -> return ClientBoundAbilities
      0x2c -> return ClientBoundCombatEvent
      0x2d -> return ClientBoundPlayerInfo
      0x2e -> return ClientBoundPosition
      0x2f -> return ClientBoundBed
      0x30 -> return ClientBoundEntityDestroy
      0x31 -> return ClientBoundRemoveEntityEffect
      0x32 -> return ClientBoundResourcePackSend
      0x33 -> return ClientBoundRespawn
      0x34 -> return ClientBoundEntityHeadRotation
      0x35 -> return ClientBoundWorldBorder
      0x36 -> return ClientBoundCamera
      0x37 -> return ClientBoundHeldItemSlot
      0x38 -> return ClientBoundScoreBoardDisplayObjective
      0x39 -> return ClientBoundEntityMetadata
      0x3a -> return ClientBoundAttachEntity
      0x3b -> return ClientBoundEntityVelocity
      0x3c -> return ClientBoundEntityEquipment
      0x3d -> return ClientBoundExperience
      0x3e -> return ClientBoundUpdateHealth
      0x3f -> return ClientBoundScoreBoardObjective
      0x40 -> return ClientBoundScoreBoardTeam
      0x41 -> return ClientBoundScoreBoardScore
      0x42 -> return ClientBoundSpawnPosition
      0x43 -> return ClientBoundUpdateTime
      0x44 -> return ClientBoundTitle
      0x45 -> return ClientBoundUpdateSign
      0x46 -> return ClientBoundPlayerlistHeader
      0x47 -> return ClientBoundCollect
      0x48 -> return ClientBoundEntityTeleport
      0x49 -> return ClientBoundUpdateAttributes
      0x4a -> return ClientBoundEntityEffect
      _    -> undefined


instance Serialize ServerBoundPlay where
  put ServerBoundKeepAlive = undefined
  put ServerBoundChat = undefined
  put ServerBoundUseEntity = undefined
  put ServerBoundFlying = undefined
  put ServerBoundPosition = undefined
  put ServerBoundLook = undefined
  put ServerBoundPositionLook = undefined
  put ServerBoundBlockDig = undefined
  put ServerBoundBlockPlace = undefined
  put ServerBoundHeldItemSlot = undefined
  put ServerBoundArmAnimation = undefined
  put ServerBoundEntityAction = undefined
  put ServerBoundSteerVehicle = undefined
  put ServerBoundCloseWindow = undefined
  put ServerBoundWindowClick = undefined
  put ServerBoundTransaction = undefined
  put ServerBoundSetCreativeSlot = undefined
  put ServerBoundEnchantItem = undefined
  put ServerBoundUpdateSign = undefined
  put ServerBoundAbilities = undefined
  put ServerBoundTabComplete = undefined
  put ServerBoundSettings = undefined
  put ServerBoundClientCommand = undefined
  put ServerBoundCustomPayload = undefined
  put ServerBoundSpectate = undefined
  put ServerBoundResourcePackReceive = undefined
  put ServerBoundUseItem = undefined

  get = do
    _ <- getWord8
    packetID <- getWord8
    case packetID of
      0x00 -> return ServerBoundTabComplete
      0x01 -> return ServerBoundChat
      0x02 -> return ServerBoundClientCommand
      0x03 -> return ServerBoundSettings
      0x04 -> return ServerBoundTransaction
      0x05 -> return ServerBoundEnchantItem
      0x06 -> return ServerBoundWindowClick
      0x07 -> return ServerBoundCloseWindow
      0x08 -> return ServerBoundCustomPayload
      0x09 -> return ServerBoundUseEntity
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
      0x1a -> return ServerBoundUseItem
      _    -> undefined

