{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Types
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Types
  ( Chat
  , Short
  , Angle
  , Position
  , NextState (..)
  , Animation (..)
  , BlockAction (..)
  , BossBarAction (..)
  , EntityMetadataEntry (..)
  , ValueField (..)
  , EntityMetadata
  , PlayerProperty (..)
  , PlayerListAction (..)
  , Statistic (..)
  , Player (..)
  , StatusPayload (..)
  , Players (..)
  , Version (..)
  , Description (..)
  , BlockChange (..)
  , Icon (..)
  , CombatEvent (..)
  , WorldBorderAction (..)
  , TeamMode (..)
  , TitleAction (..)
  , Slot
  , EntityProperty (..)
  , ChunkSection (..)
  , VarInt
  , VarLong
  , UpdateBlockEntityAction (..)
  , EntityStatus (..)
  , GameChangeReason (..)
  , WindowProperty (..)
  , decodeWord16BE
  , decodeWord32BE
  , decodeWord64BE
  , decodeInt8
  , decodeInt16BE
  , decodeInt32BE
  , decodeInt64BE
  , decodeFloatBE
  , decodeDoubleBE
  , encodeVarInt
  , decodeVarInt
  , encodeVarLong
  , decodeVarLong
  , encodeText
  , decodeText
  , encodeUUID
  , decodeUUID
  , encodeAngle
  , decodeAngle
  , encodeEntityMetadata
  , decodeEntityMetadata
  , encodePosition
  , decodePosition
  , encodeStatistic
  , decodeStatistic
  , encodeNBT
  , decodeNBT
  , encodeVector
  , decodeVector
  , encodeRecord
  , decodeRecord
  , encodeSlot
  , decodeSlot
  , encodeChunkSection
  , decodeChunkSection
  , encodeIcon
  , decodeIcon
  , encodePlayer
  , decodePlayer
  , encodeEntityProperty
  , decodeEntityProperty
  , encodeByteString
  , decodeByteString
  ) where

import            Prelude hiding (max)
import qualified  Data.Aeson as Aeson
import qualified  Data.Attoparsec.ByteString as Decode
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Unsafe as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as BB
import            Data.Int
import            Data.List
import            Data.Maybe
import            Data.Monoid
import            Data.NBT
import            Data.Serialize
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID
import qualified  Data.Vector as V
import            Data.Word
import            GHC.Generics

import OpenSandbox.Types


type Chat = T.Text


type Short = Int16


type Angle = Word8


type Position = Word64


type VarInt = Int


type VarLong = Int64


data NextState = ProtocolStatus | ProtocolLogin
  deriving (Show,Eq)


instance Enum NextState where
  fromEnum ProtocolStatus = 1
  fromEnum ProtocolLogin = 2
  toEnum 1 = ProtocolStatus
  toEnum 2 = ProtocolLogin
  toEnum _ = undefined


data BlockChange = BlockChange
  { hPosition     :: !Word8
  , yCoord        :: !Word8
  , blockId       :: !Int
  } deriving (Show,Eq)


data Animation
  = SwingArm
  | TakeDamage
  | LeaveBed
  | EatFood
  | CriticalEffect
  | MagicCriticalEffect
  deriving (Show,Eq,Enum)


data UpdateBlockEntityAction
  = SetSpawnPotentials
  | SetCommandBlockText
  | SetBeacon
  | SetModHead
  | SetFlower
  | SetBanner
  | SetStuctureTileEntity
  | SetGateway
  | SetSign
  deriving (Show,Eq)


instance Enum UpdateBlockEntityAction where
  fromEnum SetSpawnPotentials = 1
  fromEnum SetCommandBlockText = 2
  fromEnum SetBeacon = 3
  fromEnum SetModHead = 4
  fromEnum SetFlower = 5
  fromEnum SetBanner = 6
  fromEnum SetStuctureTileEntity = 7
  fromEnum SetGateway = 8
  fromEnum SetSign = 9
  fromEnum _ = undefined

  toEnum 1 = SetSpawnPotentials
  toEnum 2 = SetCommandBlockText
  toEnum 3 = SetBeacon
  toEnum 4 = SetModHead
  toEnum 5 = SetFlower
  toEnum 6 = SetBanner
  toEnum 7 = SetStuctureTileEntity
  toEnum 8 = SetGateway
  toEnum 9 = SetSign
  toEnum _ = undefined


data BlockAction
  = NoteBlockAction InstrumentType NotePitch
  | PistonBlockAction PistonState PistonDirection
  | ChestBlockAction Word8
  deriving (Show,Eq)


data InstrumentType
  = Harp
  | DoubleBass
  | SnareDrum
  | Clicks
  | BassDrum
  deriving (Show,Eq,Enum)


data NotePitch
  = FSharp
  | G
  | GSharp
  | A
  | ASharp
  | B
  | C
  | CSharp
  | D
  | DSharp
  | E
  | F
  | FSharp2
  | G2
  | GSharp2
  | A2
  | ASharp2
  | B2
  | C2
  | CSharp2
  | D2
  | DSharp2
  | E2
  | F2
  | FSharp3
  deriving (Show,Eq,Enum)


data PistonState
  = PistonPushing
  | PistonPulling
  deriving (Show,Eq,Enum)


data PistonDirection
  = PistonDown
  | PistonUp
  | PistonSouth
  | PistonWest
  | PistonNorth
  | PistonEast
  deriving (Show,Eq,Enum)


data BossBarAction
  = BossBarAdd Chat Float Int Int Word8
  | BossBarRemove
  | BossBarUpdateHealth Float
  | BossBarUpdateTitle Chat
  | BossBarUpdateStyle Int Int
  | BossBarUpdateFlags Word8
  deriving (Show,Eq)


data WindowProperty
  = WindowFurnace FurnaceProperty
  | WindowEnchantmentTable EnchantmentTableProperty
  | WindowBeacon BeaconProperty
  | WindowAnvil AnvilProperty
  | WindowBrewingStand BrewingStandProperty
  deriving (Show,Eq)


data FurnaceProperty
  = FireIcon
  | MaxBurnTime
  | ProgressArrow
  | MaxProgress
  deriving (Show,Eq,Enum)


data EnchantmentTableProperty
  = LvlReqTopSlot
  | LvlReqMiddleSlot
  | LvlReqBottomSlot
  | Seed
  | TopMouseHover
  | MiddleMouseHover
  | BottomMouseHover
  deriving (Show,Eq,Enum)


data BeaconProperty
  = PowerLevel
  | FirstPotionEffect
  | SecondPotionEffect
  deriving (Show,Eq,Enum)


data AnvilProperty
  = RepairCost
  deriving (Show,Eq,Enum)


data BrewingStandProperty
  = BrewTime
  deriving (Show,Eq,Enum)


data EntityStatus
  = TippedArrowParticles
  | ResetMobSpawnerMinecartTimer
  | RabbitRunningParticles
  | EntityHurt
  | EntityDied
  | IronGolemThrowingUpArms
  | SpawnTamingParticles
  | SpawnTamedParticles
  | WolfShakingWater
  | FinishItemUse
  | SheepEatingGrass
  | IgniteTNTMinecart
  | IronGolemHandingOverRose
  | VillageMating
  | AngryVillagerParticles
  | HappyVillagerParticles
  | WitchAnimation
  | ZombieConvertingToVillager
  | FireworkExploding
  | AnimalInLove
  | ResetSquidRotation
  | SpawnExplosionParticles
  | PlayGuardianSound
  | EnableReducedDebug
  | DisableReducedDebug
  | SetOpPermissionLvl0
  | SetOpPermissionLvl1
  | SetOpPermissionLvl2
  | SetOpPermissionLvl3
  | SetOpPermissionLvl4
  | ShieldBlockSound
  | ShieldBreakSound
  | FishingRodBobber
  | ArmorstandHitSound
  | EntityHurtFromThorns
  deriving (Show,Eq,Enum)


data GameChangeReason
  = InvalidBed
  | EndRaining
  | BeginRaining
  | ChangeGameMode
  | ExitEnd
  | DemoMessage
  | ArrowHittingPlayer
  | FadeValue
  | FateTime
  | PlayElderGuardianAppearance
  deriving (Show,Eq,Enum)


data EffectID
  = DispenserDispenses
  | DispenserFails
  | DispenserShoots
  | EnderEyeLaunched
  | FireworkShot
  | IronDoorOpened
  | WoodenDoorOpened
  | WoodenTrapdoorOpened
  | FenceGateOpened
  | FireExtinguished
  | PlayRecord
  | IronDoorClosed
  | WoodenDoorClosed
  | WoodenTrapdoorClosed
  | FenceGateClosed
  | GhastWarns
  | GhastShoots
  | EnderdragonShoots
  | BlazeShoots
  deriving (Show,Eq)


data SmokeDirection
  = SouthEast
  | South
  | SouthWest
  | East
  | Up
  | West
  | NorthEast
  | North
  | NorthWest
  deriving (Show,Eq,Enum)


data ParticleID
  = Explode
  | LargeExplosion
  | HugeExplosion
  | FireworksSpark
  | Bubble
  | Splash
  | Wake
  | Suspended
  | DepthSuspend
  | Crit
  | MagicCrit
  | Smoke
  | LargeSmoke
  | Spell
  | InstantSpell
  | MobSpell
  | MobSpellAmbient
  | WitchMagic
  | DripWater
  | DripLava
  | AngryVillager
  | HappyVillager
  | TownAura
  | Note
  | Portal
  | EnchantmentTable
  | Flame
  | Lava
  | Footstep
  | Cloud
  | RedDust
  | SnowballPoof
  | SnowShovel
  | Slime
  | Heart
  | Barrier
  | IconCrack
  | BlockCrack
  | BlockDust
  | Droplet
  | Take
  | MobAppearance
  | DragonBreath
  | Endrod
  | DamageIndicator
  | SweepAttack
  deriving (Show,Eq,Enum)


data EntityMetadataEntry = Entry
  { entryIndex  :: Word8
  , entryType   :: Maybe Word8
  , entryValue  :: Maybe Word8
  } deriving (Show,Eq)


data ValueField
  = ByteField
  | VarIntField
  | FloatField
  | StringField
  | ChatField
  | SlotField
  | BoolField
  | Vector3FField
  | PositionField
  | OptPositionField
  | DirectionField
  | OptUUIDField
  | BlockIDField
  deriving (Show,Eq)


instance Enum ValueField where
  fromEnum ByteField = 0
  fromEnum VarIntField = 1
  fromEnum FloatField = 2
  fromEnum StringField = 3
  fromEnum ChatField = 4
  fromEnum SlotField = 5
  fromEnum BoolField = 6
  fromEnum Vector3FField = 7
  fromEnum PositionField = 8
  fromEnum OptPositionField = 9
  fromEnum DirectionField = 10
  fromEnum OptUUIDField = 11
  fromEnum BlockIDField = 12
  toEnum 0 = ByteField
  toEnum 1 = VarIntField
  toEnum 2 = FloatField
  toEnum 3 = StringField
  toEnum 4 = ChatField
  toEnum 5 = SlotField
  toEnum 6 = BoolField
  toEnum 7 = Vector3FField
  toEnum 8 = PositionField
  toEnum 9 = OptPositionField
  toEnum 10 = DirectionField
  toEnum 11 = OptUUIDField
  toEnum 12 = BlockIDField
  toEnum _ = undefined


type EntityMetadata = V.Vector EntityMetadataEntry


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


data ChunkSection = ChunkSection
  { bitsPerBlock  :: !Word8
  , palette       :: !(Maybe (V.Vector Int))
  , dataArray     :: !B.ByteString
  , blockLight    :: !B.ByteString
  , skyLight      :: !(Maybe B.ByteString)
  } deriving (Show,Eq)


data Statistic = Statistic T.Text VarInt deriving (Show,Eq)


data Player = Player
  { playerUUID        :: UUID
  , playerListAction  :: PlayerListAction
  } deriving (Show,Eq)


data PlayerListAction
  = PlayerListAdd T.Text (V.Vector PlayerProperty) GameMode Int (Maybe T.Text)
  | PlayerListUpdateGameMode GameMode
  | PlayerListUpdateLatency Int
  | PlayerListUpdateDisplayName Bool (Maybe T.Text)
  | PlayerListRemovePlayer
  deriving (Show,Eq)


data PlayerProperty = PlayerProperty
  { playerName    :: !T.Text
  , playerValue   :: !T.Text
  , isSigned      :: !Bool
  , playerSig     :: !(Maybe T.Text)
  } deriving (Show,Eq)


data Icon = Icon
  { directionAndType  :: !Word8
  , x                 :: !Word8
  , z                 :: !Word8
  } deriving (Show,Eq)


data CombatEvent
  = EnterCombat
  | EndCombat Int Int32
  | EntityDead Int Int32 Chat
  deriving (Show,Eq)


data WorldBorderAction
  = SetSize Double
  | LerpSize Double Double Int64
  | SetCenter Double Double
  | Initialize Double Double Double Double Int64 Int Int Int
  | SetWarningTime Int
  | SetWarningBlocks Int
  deriving (Show,Eq)


data TeamMode
  = CreateTeam T.Text T.Text T.Text Int8 T.Text T.Text Int8 (V.Vector T.Text)
  | RemoveTeam
  | UpdateTeamInfo T.Text T.Text T.Text Int8 T.Text T.Text Int8
  | AddPlayers (V.Vector T.Text)
  | RemovePlayers (V.Vector T.Text)
  deriving (Show,Eq)


data TitleAction
  = SetTitle Chat
  | SetSubtitle Chat
  | SetTimesAndDisplay Int32 Int32 Int32
  | Hide
  | Reset
  deriving (Show,Eq)


type Slot = NBT


data EntityProperty = EntityProperty
  { key             :: !T.Text
  , value           :: !Double
  , numOfModifiers  :: !Int
  , modifiers       :: !Int
  } deriving (Show,Eq)


encodeVarInt :: VarInt -> BB.Builder
encodeVarInt i
    | i < 0     = encodeVarInt ((abs i) + (2^31 :: Int))
    | i < 0x80  = BB.word8 (fromIntegral i)
    | otherwise = BB.word8 (fromIntegral (i .&. 0x7F) .|. 0x80) <> encodeVarInt (i `shiftR` 7)
{-# INLINE encodeVarInt #-}


decodeVarInt :: Decode.Parser VarInt
decodeVarInt = do
    w <- Decode.anyWord8
    if testBit w 7
      then do
        result <- go 7 (fromIntegral (w .&. 0x7F))
        if result <= (2^31)
          then return result
          else return (0 - (result - (2^31)))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- Decode.anyWord8
      if testBit w' 7
        then go (n+7) (val .|. ((fromIntegral (w' .&. 0x7F)) `shiftL` n))
        else return (val .|. ((fromIntegral w') `shiftL` n))
{-# INLINE decodeVarInt #-}


encodeVarLong :: Int64 -> BB.Builder
encodeVarLong l = undefined


decodeVarLong :: Decode.Parser VarLong
decodeVarLong = undefined


decodeFloatBE :: Decode.Parser Float
decodeFloatBE = undefined


decodeDoubleBE :: Decode.Parser Double
decodeDoubleBE = undefined


encodeText :: T.Text -> BB.Builder
encodeText t =
  (encodeVarInt . B.length . encodeUtf8 $ t)
  <> (BB.byteString . encodeUtf8 $ t)
{-# INLINE encodeText #-}


decodeText :: Decode.Parser T.Text
decodeText = do
  ln <- decodeVarInt
  if ln /= 0
    then fmap decodeUtf8 (Decode.take ln)
    else return ""
{-# INLINE decodeText #-}


encodeUUID :: UUID -> BB.Builder
encodeUUID u = encodeText (toText u)
{-# INLINE encodeUUID #-}


decodeUUID :: Decode.Parser UUID
decodeUUID = do
  txt <- decodeText
  case fromText txt of
    Just uuid -> return uuid
    Nothing   -> fail "Error: Could not decode UUID!"
{-# INLINE decodeUUID #-}


decodeWord16BE :: Decode.Parser Word16
decodeWord16BE = undefined


decodeWord32BE :: Decode.Parser Word32
decodeWord32BE = undefined


decodeWord64BE :: Decode.Parser Word64
decodeWord64BE = undefined


decodeInt8 :: Decode.Parser Int8
decodeInt8 = do
  bs <- Decode.take 1
  return $! fromIntegral (B.unsafeHead bs)


decodeInt16BE :: Decode.Parser Int16
decodeInt16BE = do
    bs <- Decode.take 2
    return $! (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 8) .|.
              (fromIntegral (bs `B.unsafeIndex` 1))


decodeInt32BE :: Decode.Parser Int32
decodeInt32BE = do
    bs <- Decode.take 4
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL`  8) .|.
      (fromIntegral (bs `B.unsafeIndex` 3))



decodeInt64BE :: Decode.Parser Int64
decodeInt64BE = do
    bs <- Decode.take 8
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 56) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 48) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL` 40) .|.
      (fromIntegral (bs `B.unsafeIndex` 3) `shiftL` 32) .|.
      (fromIntegral (bs `B.unsafeIndex` 4) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 5) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 6) `shiftL`  8) .|.
      (fromIntegral (bs `B.unsafeIndex` 7))


encodeAngle :: Angle -> BB.Builder
encodeAngle a = undefined


decodeAngle :: Decode.Parser Angle
decodeAngle = undefined


encodeEntityMetadata :: EntityMetadata -> BB.Builder
encodeEntityMetadata e = undefined


decodeEntityMetadata :: Decode.Parser EntityMetadata
decodeEntityMetadata = undefined


encodePosition :: Position -> BB.Builder
encodePosition p = undefined


decodePosition :: Decode.Parser Position
decodePosition = undefined


encodeStatistic :: Statistic -> BB.Builder
encodeStatistic s = undefined


decodeStatistic :: Decode.Parser Statistic
decodeStatistic = undefined


encodeNBT :: NBT -> BB.Builder
encodeNBT n = undefined


decodeNBT :: Decode.Parser NBT
decodeNBT = undefined


encodeVector :: V.Vector a -> BB.Builder
encodeVector v = undefined


decodeVector :: Decode.Parser (V.Vector a)
decodeVector = undefined


encodeRecord :: BlockChange -> BB.Builder
encodeRecord m =
  (BB.word8 . hPosition $ m)
  <> (BB.word8 . yCoord $ m)
  <> (encodeVarInt . toEnum . blockId $ m)


decodeRecord :: Decode.Parser BlockChange
decodeRecord = undefined


encodeSlot :: Slot -> BB.Builder
encodeSlot s = undefined


decodeSlot :: Decode.Parser Slot
decodeSlot = undefined


encodeChunkSection :: ChunkSection -> BB.Builder
encodeChunkSection c = undefined


decodeChunkSection :: Decode.Parser ChunkSection
decodeChunkSection = undefined


encodeIcon :: Icon -> BB.Builder
encodeIcon i = undefined


decodeIcon :: Decode.Parser Icon
decodeIcon = undefined


encodePlayer :: Player -> BB.Builder
encodePlayer p = undefined


decodePlayer :: Decode.Parser Player
decodePlayer = undefined


encodeEntityProperty :: EntityProperty -> BB.Builder
encodeEntityProperty e = undefined


decodeEntityProperty :: Decode.Parser EntityProperty
decodeEntityProperty = undefined

encodeByteString :: B.ByteString -> BB.Builder
encodeByteString b =
  (encodeVarInt . toEnum . B.length $ b)
  <> BB.byteString b

decodeByteString :: Decode.Parser B.ByteString
decodeByteString = do
  len <- fmap fromEnum decodeVarInt
  Decode.take len
