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
module OpenSandbox.Minecraft.Protocol.Play where

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
  | ClientBoundHeadRotation
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
  | ClientBoundClose
  | ClientBoundSetSlot
  | ClientBoundWindowItems
  | ClientBoundCraftProgressBar
  | ClientBoundTransactions
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
  | ClientBoundSetCompression
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
  put ClientBoundHeadRotation = undefined
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
  put ClientBoundClose = undefined
  put ClientBoundSetSlot = undefined
  put ClientBoundWindowItems = undefined
  put ClientBoundCraftProgressBar = undefined
  put ClientBoundTransactions = undefined
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
  put ClientBoundSetCompression = undefined
  put ClientBoundPlayerlistHeader = undefined
  put ClientBoundResourcePackSend = undefined
  put ClientBoundBossBar = undefined
  put ClientBoundSetCooldown = undefined
  put ClientBoundUnloadChunk = undefined
