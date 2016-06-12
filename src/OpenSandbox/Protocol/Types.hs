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
  --, Description (..)
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
  , UpdateBlockEntityAction (..)
  , EntityStatus (..)
  , GameChangeReason (..)
  , putVarInt
  , getVarInt
  , putByteStringField
  , putPosition
  , putText
  , getText
  , putBool
  , putSlot
  , putUUID
  , getUUID
  ) where

import            Prelude hiding (max)
import qualified  Data.Aeson as Aeson
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import            Data.Int
import            Data.Maybe
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

type Short = Word16

type Angle = Word8

type Position = Word64

type VarInt = Int


data NextState = ProtocolStatus | ProtocolLogin deriving (Show,Eq)

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


data WindowType
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


instance Serialize Statistic where
  put (Statistic statName statVal) = do
    putByteStringField . encodeUtf8 $ statName
    putVarInt statVal

  get = Statistic <$> fmap decodeUtf8 (getVarInt >>= getByteString) <*> getVarInt


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
  = PlayerListAdd T.Text (V.Vector PlayerProperty) GameMode Int (Maybe T.Text)
  | PlayerListUpdateGameMode GameMode
  | PlayerListUpdateLatency Int
  | PlayerListUpdateDisplayName Bool (Maybe T.Text)
  | PlayerListRemovePlayer
  deriving (Show,Eq)


instance Serialize PlayerListAction where
  put (PlayerListAdd name properties gameMode ping displayName) = do
    let namePayload = encodeUtf8 name
    let nameLen = B.length namePayload
    putVarInt nameLen
    putByteString namePayload
    put properties
    putVarInt . fromEnum $ gameMode
    putVarInt ping
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
  = CreateTeam T.Text T.Text T.Text Word8 T.Text T.Text Word8 Int (V.Vector T.Text)
  | RemoveTeam
  | UpdateTeamInfo T.Text T.Text T.Text Word8 T.Text T.Text Word8
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

-- Adapted from the protocol-buffers library, but only for Serialize and Ints

putVarInt :: Int -> Put
putVarInt i | i < 0x80 = putWord8 (fromIntegral i)
            | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> putVarInt (i `shiftR` 7)
{-# INLINE putVarInt #-}


getVarInt :: Get Int
getVarInt = do
    w <- getWord8
    if testBit w 7
      then go 7 (fromIntegral (w .&. 0x7F))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- getWord8
      if testBit w' 7
        then go (n+7) (val .|. ((fromIntegral (w' .&. 0x7F)) `shiftL` n))
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

putPosition :: Position -> PutM ()
putPosition p = putWord64be p
{-# INLINE putPosition #-}

putText :: T.Text -> PutM ()
putText "" = do
  putVarInt 0
putText t = do
  let bs = encodeUtf8 t
  putVarInt . B.length $ bs
  putByteString bs
{-# INLINE putText #-}

getText :: Get T.Text
getText = do
  i <- getVarInt
  fmap decodeUtf8 $ getByteString i
{-# INLINE getText #-}

putBool :: Bool -> PutM ()
putBool b = put b
{-# INLINE putBool #-}

putSlot :: Slot -> PutM ()
putSlot = put
{-# INLINE putSlot #-}

putUUID :: UUID -> PutM ()
putUUID = putByteString . toASCIIBytes
{-# INLINE putUUID #-}

getUUID :: Get UUID
getUUID = undefined
{-# INLINE getUUID #-}
