{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , DifficultyField (..)
  , DimensionField (..)
  , GameModeField (..)
  , Animation (..)
  , BlockAction (..)
  , InstrumentType (..)
  , NotePitch (..)
  , PistonState (..)
  , PistonDirection (..)
  , BossBarAction (..)
  , EntityMetadataEntry (..)
  , ValueField (..)
  , EntityMetadata
  , PlayerProperty (..)
  , PlayerListAction (..)
  , Statistic (..)
  , PlayerListEntry (..)
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
  , Slot (..)
  , SlotData (..)
  , EntityProperty (..)
  , ChunkSection (..)
  , VarInt
  , VarLong
  , UpdateBlockEntityAction (..)
  , EntityStatus (..)
  , GameChangeReason (..)
  , WindowProperty (..)
  , NBT (..)
  , NamelessNBT (..)
  , NBTList (..)
  , TagType (..)
  , UpdatedColumns (..)
  , UpdateScoreAction (..)
  , UseEntityType (..)
  , EntityHand (..)
  , mkSlot
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
  , encodeBool
  , decodeBool
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
  , encodeRecord
  , decodeRecord
  , encodeSlot
  , decodeSlot
  , encodeChunkSection
  , decodeChunkSection
  , encodeIcon
  , decodeIcon
  , encodePlayerListEntry
  , decodePlayerListEntry
  , encodeEntityProperty
  , decodeEntityProperty
  , encodeByteString
  , decodeByteString
  , encodeStatusPayload
  ) where

import            Prelude hiding (max)
import            Control.Monad.ST (runST,ST)
import            Data.Aeson as Aeson
import qualified  Data.Attoparsec.ByteString as Decode
import            Data.Array.MArray (MArray,readArray,newArray)
import            Data.Array.ST
import            Data.Array.Unsafe (castSTUArray)
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Unsafe as B
import qualified  Data.ByteString.Builder as Encode
import            Data.Int
import            Data.Maybe
import            Data.Monoid
import            Data.NBT.Encode (encodeNBT)
import            Data.NBT.Decode (decodeNBT)
import            Data.NBT.Types (NBT (..), NamelessNBT (..),NBTList (..),TagType(..))
import            Data.Serialize (encode,decode)
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID
import qualified  Data.Vector as V
import            Data.Word
import            GHC.Generics
import            OpenSandbox.Types
import            Prelude hiding (max)


-------------------------------------------------------------------------------
-- | Core Protocol Types
-------------------------------------------------------------------------------

-- Bool
encodeBool :: Bool -> Encode.Builder
encodeBool b = Encode.word8 (toEnum . fromEnum $ b)

decodeBool :: Decode.Parser Bool
decodeBool = fmap (toEnum . fromEnum) Decode.anyWord8

-- Byte
decodeInt8 :: Decode.Parser Int8
decodeInt8 = do
  bs <- Decode.take 1
  return $! fromIntegral (B.unsafeHead bs)


-- Unsigned Byte
-- native


-- Short
type Short = Int16

decodeInt16BE :: Decode.Parser Short
decodeInt16BE = do
    bs <- Decode.take 2
    return $! (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 8) .|.
              (fromIntegral (bs `B.unsafeIndex` 1))

-- Unsigned Short
decodeWord16BE :: Decode.Parser Word16
decodeWord16BE = do
    bs <- Decode.take 2
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 8) .|.
      (fromIntegral (bs `B.unsafeIndex` 1))

-- Int
decodeInt32BE :: Decode.Parser Int32
decodeInt32BE = do
    bs <- Decode.take 4
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL`  8) .|.
      (fromIntegral (bs `B.unsafeIndex` 3))

-- Long
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

-- Float
decodeFloatBE :: Decode.Parser Float
decodeFloatBE = wordToFloat <$> decodeWord32BE

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

-- Double
decodeDoubleBE :: Decode.Parser Double
decodeDoubleBE = wordToDouble <$> decodeWord64BE

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

-- String
encodeText :: T.Text -> Encode.Builder
encodeText t =
  (encodeVarInt . B.length . encodeUtf8 $ t)
  <> (Encode.byteString . encodeUtf8 $ t)

decodeText :: Decode.Parser T.Text
decodeText = do
  ln <- decodeVarInt
  if ln /= 0
    then fmap decodeUtf8 (Decode.take ln)
    else return ""

-- Chat
type Chat = T.Text

-- VarInt ---------------------------------------------------------------------
type VarInt = Int

encodeVarInt :: VarInt -> Encode.Builder
encodeVarInt i
    | i < 0     = encodeVarInt ((abs i) + (2^31 :: Int))
    | i < 0x80  = Encode.word8 (fromIntegral i)
    | otherwise = Encode.word8 (fromIntegral (i .&. 0x7F) .|. 0x80) <> encodeVarInt (i `shiftR` 7)

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

-- VarLong --------------------------------------------------------------------
type VarLong = Int64

encodeVarLong :: VarLong -> Encode.Builder
encodeVarLong i
    | i < 0     = encodeVarLong ((abs i) + (2^63 :: Int64))
    | i < 0x80  = Encode.word8 (fromIntegral i)
    | otherwise = Encode.word8 (fromIntegral (i .&. 0x7F) .|. 0x80) <> encodeVarLong (i `shiftR` 7)

decodeVarLong :: Decode.Parser VarLong
decodeVarLong = do
    w <- Decode.anyWord8
    if testBit w 7
      then do
        result <- go 7 (fromIntegral (w .&. 0x7F))
        if result <= (2^63)
          then return result
          else return (0 - (result - (2^63)))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- Decode.anyWord8
      if testBit w' 7
        then go (n+7) (val .|. ((fromIntegral (w' .&. 0x7F)) `shiftL` n))
        else return (val .|. ((fromIntegral w') `shiftL` n))

-- Chunk Section: 16x16x16 area
-- Chunk Column: 16 chunks aligned vertically
data ChunkSection = ChunkSection
  { bitsPerBlock  :: !Word8
  , palette       :: !(V.Vector Int)
  , dataArray     :: !(V.Vector Int64)
  , blockLight    :: !B.ByteString
  , skyLight      :: !(Maybe B.ByteString)
  } deriving (Show,Eq)

encodeChunkSection :: ChunkSection -> Encode.Builder
encodeChunkSection (ChunkSection a b c d e) =
  Encode.word8 a
  <> encodeVarInt (V.length b)
  <> (V.foldl' (<>) mempty (fmap encodeVarInt b))
  <> encodeVarInt (V.length c)
  <> (V.foldl' (<>) mempty (fmap Encode.int64BE c))
  <> Encode.byteString d
  <> (case e of
      Just bs -> Encode.byteString bs
      Nothing -> mempty
    )

decodeChunkSection :: Int -> Decode.Parser ChunkSection
decodeChunkSection bitmask = do
  bitsPerBlock <- Decode.anyWord8
  paletteCount <- decodeVarInt
  palette <- V.replicateM paletteCount decodeVarInt
  dataCount <- decodeVarInt
  dataArray <- V.replicateM dataCount decodeInt64BE
  blockLight <- Decode.take 256
  if bitmask > 255
    then do
      skyLight <- Decode.take 256
      return $ ChunkSection bitsPerBlock palette dataArray blockLight (Just skyLight)
    else return $ ChunkSection bitsPerBlock palette dataArray blockLight Nothing

-- Entity Metadata
type EntityMetadata = V.Vector EntityMetadataEntry

encodeEntityMetadata :: EntityMetadata -> Encode.Builder
encodeEntityMetadata e = V.foldl' (<>) mempty (fmap encodeEntityMetadataEntry e)

decodeEntityMetadata :: Decode.Parser EntityMetadata
decodeEntityMetadata = do
  count <- decodeVarInt
  V.replicateM count decodeEntityMetadataEntry

data EntityMetadataEntry = Entry
  { entryIndex  :: Word8
  , entryType   :: Maybe Word8
  , entryValue  :: Maybe Word8
  } deriving (Show,Eq)

mkEntityMetadataEntry :: Word8 -> Word8 -> Word8 -> EntityMetadataEntry
mkEntityMetadataEntry i t v =
  case i of
    0xff -> Entry i Nothing Nothing
    _   -> Entry i (Just t) (Just v)

encodeEntityMetadataEntry :: EntityMetadataEntry -> Encode.Builder
encodeEntityMetadataEntry e =
  Encode.word8 (entryIndex e)
  <> case (entryIndex e) of
      0xff -> mempty
      _   -> Encode.word8 (fromJust . entryType $ e)
              <> Encode.word8 (fromJust . entryValue $ e)

decodeEntityMetadataEntry :: Decode.Parser EntityMetadataEntry
decodeEntityMetadataEntry = do
  i <- Decode.anyWord8
  case i of
    0xff -> return $ Entry i Nothing Nothing
    _ -> do
      t <- Decode.anyWord8
      v <- Decode.anyWord8
      return $ Entry i (Just t) (Just v)

-- Slot
data Slot = Slot
  { blockID     :: !Int16
  , slotData    :: !(Maybe SlotData)
  } deriving (Show,Eq)

data SlotData = SlotData
  { itemCount   :: !Int8
  , itemDamage  :: !Int16
  , nbt         :: !NBT
  } deriving (Show,Eq)

mkSlot :: Int16 -> Int8 -> Int16 -> NBT -> Slot
mkSlot blockID itemCount itemDamage nbt =
  case blockID of
    -1 -> Slot blockID Nothing
    _ -> Slot blockID (Just (SlotData itemCount itemDamage nbt))

encodeSlot :: Slot -> Encode.Builder
encodeSlot (Slot blockID slotDat) =
    Encode.int16BE blockID
    <> case blockID of
          (-1) -> mempty
          _ -> Encode.int8 (itemCount slotDat')
                <> (Encode.int16BE (itemDamage slotDat'))
                <> (encodeNBT (nbt slotDat'))
  where slotDat' = fromJust slotDat

decodeSlot :: Decode.Parser Slot
decodeSlot = do
  blockID <- decodeInt16BE
  case blockID of
    -1 -> return $ Slot blockID Nothing
    _ -> do
      itemCount <- decodeInt8
      itemDamage <- decodeInt16BE
      nbt <- decodeNBT
      return $ Slot blockID (Just (SlotData itemCount itemDamage nbt))

-- NBT Tag
-- nbt library

-- Position -------------------------------------------------------------------
type Position = Word64

encodePosition :: Position -> Encode.Builder
encodePosition = Encode.word64BE

decodePosition :: Decode.Parser Position
decodePosition = decodeWord64BE

-- Angle ----------------------------------------------------------------------
type Angle = Word8

encodeAngle :: Angle -> Encode.Builder
encodeAngle a = Encode.word8 a

decodeAngle :: Decode.Parser Angle
decodeAngle = Decode.anyWord8

-- UUID -----------------------------------------------------------------------
encodeUUID :: UUID -> Encode.Builder
encodeUUID u = encodeText (toText u)

decodeUUID :: Decode.Parser UUID
decodeUUID = do
  txt <- decodeText
  case fromText txt of
    Just uuid -> return uuid
    Nothing   -> fail "Error: Could not decode UUID!"

-- ByteArray ------------------------------------------------------------------
encodeByteString :: B.ByteString -> Encode.Builder
encodeByteString b =
  (encodeVarInt . toEnum . B.length $ b)
  <> Encode.byteString b

decodeByteString :: Decode.Parser B.ByteString
decodeByteString = do
  len <- fmap fromEnum decodeVarInt
  Decode.take len

-------------------------------------------------------------------------------
-- | Protocol Enums
-------------------------------------------------------------------------------

-- Animation
data Animation
  = SwingArm
  | TakeDamage
  | LeaveBed
  | EatFood
  | CriticalEffect
  | MagicCriticalEffect
  deriving (Show,Eq,Enum)

-- BlockAction
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

-- BlockChange
data BlockChange = BlockChange
  { hPosition     :: !Word8
  , yCoord        :: !Word8
  , blockId       :: !Int
  } deriving (Show,Eq)

encodeRecord :: BlockChange -> Encode.Builder
encodeRecord m =
  (Encode.word8 . hPosition $ m)
  <> (Encode.word8 . yCoord $ m)
  <> (encodeVarInt . toEnum . blockId $ m)

decodeRecord :: Decode.Parser BlockChange
decodeRecord = do
  hp <- Decode.anyWord8
  yc <- Decode.anyWord8
  bid <- decodeVarInt
  return $ BlockChange hp yc bid

-- BossBarAction
data BossBarAction
  = BossBarAdd Chat Float Int Int Word8
  | BossBarRemove
  | BossBarUpdateHealth Float
  | BossBarUpdateTitle Chat
  | BossBarUpdateStyle Int Int
  | BossBarUpdateFlags Word8
  deriving (Show,Eq)

-- DifficultyField
data DifficultyField = PeacefulField | EasyField | NormalField | HardField
  deriving (Show,Enum,Eq,Generic)

instance FromJSON DifficultyField
instance ToJSON DifficultyField

-- DimensionField
data DimensionField = OverworldField | NetherField | EndField
  deriving (Show,Eq,Generic)

instance Enum DimensionField where
    fromEnum OverworldField = 0
    fromEnum NetherField = -1
    fromEnum EndField = 1
    toEnum 0 = OverworldField
    toEnum (-1) = NetherField
    toEnum 1 = EndField

-- GameModeField
data GameModeField = SurvivalField | CreativeField | AdventureField | SpectatorField
  deriving (Show,Enum,Eq,Generic)

instance FromJSON GameModeField
instance ToJSON GameModeField

-- NextState
data NextState = ProtocolHandshake | ProtocolStatus | ProtocolLogin | ProtocolPlay
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

  toEnum 1 = SetSpawnPotentials
  toEnum 2 = SetCommandBlockText
  toEnum 3 = SetBeacon
  toEnum 4 = SetModHead
  toEnum 5 = SetFlower
  toEnum 6 = SetBanner
  toEnum 7 = SetStuctureTileEntity
  toEnum 8 = SetGateway
  toEnum 9 = SetSign

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
  deriving (Show,Eq,Enum)

data StatusPayload = StatusPayload
  { version       :: Version
  , players       :: Players
  , description   :: Description
  } deriving (Generic,Show,Eq,Read)

instance ToJSON StatusPayload
instance FromJSON StatusPayload

data Version = Version
  { name      :: T.Text
  , protocol  :: Word8
  } deriving (Generic,Eq,Show,Read)

instance ToJSON Version
instance FromJSON Version

data Players = Players
  { max     :: Word8
  , online  :: Word8
  } deriving (Generic,Eq,Show,Read)

instance ToJSON Players
instance FromJSON Players

data Description = Description
  { text    :: T.Text
  } deriving (Generic,Eq,Show,Read)

instance ToJSON Description
instance FromJSON Description

data Statistic = Statistic T.Text VarInt deriving (Show,Eq)

encodeStatistic :: Statistic -> Encode.Builder
encodeStatistic (Statistic t val) =
  encodeText t
  <> encodeVarInt val

decodeStatistic :: Decode.Parser Statistic
decodeStatistic = do
  t <- decodeText
  v <- decodeVarInt
  return $ Statistic t v

data PlayerListEntry = PlayerListEntry
  { playerUUID        :: UUID
  , playerListAction  :: PlayerListAction
  } deriving (Show,Eq)

encodePlayerListEntry :: Int -> PlayerListEntry -> Encode.Builder
encodePlayerListEntry  a (PlayerListEntry u pla) =
  encodeUUID u
  <> encodePlayerListAction pla

decodePlayerListEntry :: Int -> Decode.Parser PlayerListEntry
decodePlayerListEntry a = PlayerListEntry <$> decodeUUID <*> decodePlayerListAction a

data PlayerListAction
  = PlayerListAdd T.Text (V.Vector PlayerProperty) GameModeField Int (Maybe T.Text)
  | PlayerListUpdateGameMode GameModeField
  | PlayerListUpdateLatency Int
  | PlayerListUpdateDisplayName (Maybe T.Text)
  | PlayerListRemovePlayer
  deriving (Show,Eq)

encodePlayerListAction :: PlayerListAction -> Encode.Builder
encodePlayerListAction (PlayerListAdd name properties gameMode ping maybeDisplayName) =
  encodeText name
  <> (encodeVarInt . V.length $ properties)
  <> (V.foldl' (<>) mempty (fmap encodePlayerProperty properties))
  <> encodeVarInt (fromEnum gameMode)
  <> encodeVarInt ping
  <> case maybeDisplayName of
      Just displayName -> encodeBool True <> encodeText displayName
      Nothing -> encodeBool False
encodePlayerListAction (PlayerListUpdateGameMode gameMode) =
  encodeVarInt (fromEnum gameMode)
encodePlayerListAction (PlayerListUpdateLatency ping) =
  encodeVarInt ping
encodePlayerListAction (PlayerListUpdateDisplayName maybeDisplayName) =
  case maybeDisplayName of
    Nothing -> encodeBool False
    Just displayName -> encodeBool True <> encodeText displayName
encodePlayerListAction PlayerListRemovePlayer =
  mempty

decodePlayerListAction :: Int -> Decode.Parser PlayerListAction
decodePlayerListAction action =
  case action of
    0 -> do
      name <- decodeText
      count <- decodeVarInt
      properties <- V.replicateM count decodePlayerProperty
      gameMode <- fmap toEnum decodeVarInt
      ping <- decodeVarInt
      hasDisplayName <- decodeBool
      if hasDisplayName
        then do
          displayName <- decodeText
          return $ PlayerListAdd name properties gameMode ping (Just displayName)
        else do
          return $ PlayerListAdd name properties gameMode ping Nothing
    1 -> PlayerListUpdateGameMode <$> (fmap toEnum decodeVarInt)
    2 -> PlayerListUpdateLatency <$> decodeVarInt
    3 -> do
      hasDisplayName <- decodeBool
      if hasDisplayName
        then do
          displayName <- decodeText
          return $ PlayerListUpdateDisplayName (Just displayName)
        else do
          return $ PlayerListUpdateDisplayName Nothing
    4 -> return PlayerListRemovePlayer
    _ -> fail "Error: Invalid PlayerListAction int!"

data PlayerProperty = PlayerProperty
  { playerName    :: !T.Text
  , playerValue   :: !T.Text
  , playerSig     :: !(Maybe T.Text)
  } deriving (Show,Eq)

encodePlayerProperty :: PlayerProperty -> Encode.Builder
encodePlayerProperty (PlayerProperty name val maybeSig) =
  encodeText name
  <> encodeText val
  <> case maybeSig of
      Nothing -> encodeBool False
      Just sig -> encodeBool True <> encodeText sig

decodePlayerProperty :: Decode.Parser PlayerProperty
decodePlayerProperty = do
  name <- decodeText
  value <- decodeText
  isSigned <- decodeBool
  if isSigned
    then do
      sig <- decodeText
      return $ PlayerProperty name value (Just sig)
    else do
      return $ PlayerProperty name value Nothing

data Icon = Icon
  { directionAndType  :: !Word8
  , x                 :: !Word8
  , z                 :: !Word8
  } deriving (Show,Eq)

encodeIcon :: Icon -> Encode.Builder
encodeIcon i =
  Encode.word8 (directionAndType i)
  <> Encode.word8 (x i)
  <> Encode.word8 (z i)

decodeIcon :: Decode.Parser Icon
decodeIcon = Icon <$> Decode.anyWord8 <*> Decode.anyWord8 <*> Decode.anyWord8

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

data EntityProperty = EntityProperty
  { key             :: !T.Text
  , value           :: !Double
  , numOfModifiers  :: !Int
  , modifiers       :: !Int
  } deriving (Show,Eq)

encodeEntityProperty :: EntityProperty -> Encode.Builder
encodeEntityProperty (EntityProperty k v n m) = do
  encodeText k
  <> Encode.doubleBE v
  <> encodeVarInt n
  <> encodeVarInt m

decodeEntityProperty :: Decode.Parser EntityProperty
decodeEntityProperty = do
  k <- decodeText
  v <- decodeDoubleBE
  n <- decodeVarInt
  m <- decodeVarInt
  return $ EntityProperty k v n m

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

decodeWord32BE :: Decode.Parser Word32
decodeWord32BE = do
    bs <- Decode.take 4
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL` 8) .|.
      (fromIntegral (bs `B.unsafeIndex` 3))

decodeWord64BE :: Decode.Parser Word64
decodeWord64BE = do
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

encodeStatusPayload :: T.Text -> Word8 -> Word8 -> Word8 -> T.Text -> Encode.Builder
encodeStatusPayload mcVersion versionID currentPlayers maxPlayers motd =
  encodeByteString . BL.toStrict . Aeson.encode $
    StatusPayload (Version mcVersion versionID)
                  (Players maxPlayers currentPlayers)
                  (Description motd)

data UpdatedColumns
  = NoUpdatedColumns
  | UpdatedColumns Int8 Int8 Int8 Int8 B.ByteString
  deriving (Show,Eq)

data UpdateScoreAction
  = CreateOrUpdateScoreItem T.Text T.Text VarInt
  | RemoveScoreItem T.Text T.Text
  deriving (Show,Eq)

data UseEntityType
  = InteractWithEntity VarInt
  | AttackEntity
  | InteractAtEntity Float Float Float EntityHand
  deriving (Show,Eq)

data EntityHand = MainHand | OffHand deriving (Show,Eq,Enum)
