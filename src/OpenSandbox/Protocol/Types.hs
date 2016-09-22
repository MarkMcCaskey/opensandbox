{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
  ( Chat (..)
  , Short
  , Angle
  , Position
  , ProtocolState (..)
  , Difficulty (..)
  , Dimension (..)
  , GameMode (..)
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
  , MetadataType (..)
  , PlayerProperty (..)
  , PlayerListEntries (..)
  , PlayerListAdd (..)
  , PlayerListUpdateGameMode (..)
  , PlayerListUpdateLatency (..)
  , PlayerListUpdateDisplayName (..)
  , PlayerListRemovePlayer (..)
  , Statistic (..)
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
  , VarInt
  , VarLong
  , UpdateBlockEntityAction (..)
  , EntityStatus (..)
  , GameChangeReason (..)
  , WindowProperty (..)
  , NBT (..)
  , UpdatedColumns (..)
  , UpdateScoreAction (..)
  , UseEntityType (..)
  , EntityHand (..)
  , ScoreboardMode (..)
  , mkSlot
  , encodeStatusPayload
  , putText
  , getText
  , putVarInt
  , getVarInt
  , putVarLong
  , getVarLong
  , putEntityMetadata
  , getEntityMetadata
  , putNetcodeByteString
  , getNetcodeByteString
  , putPosition
  , getPosition
  , putAngle
  , getAngle
  , putUUID
  , getUUID
  , putUUID'
  , getUUID'
  ) where

import Control.DeepSeq
import Control.Monad
import Crypto.PubKey.RSA
import qualified Data.Aeson as A
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.List
import Data.Maybe
import Data.NBT
import Data.Serialize
import qualified Data.Text as T
import Data.Text.Encoding
import Data.UUID
import qualified Data.Vector as V
import Data.Word
import GHC.Generics
import Prelude hiding (max)

type Short = Int16

putText :: T.Text -> Put
putText t = do
  putVarInt . B.length . encodeUtf8 $ t
  putByteString . encodeUtf8 $ t

getText :: Get T.Text
getText = do
  ln <- getVarInt
  bs <- getBytes ln
  return $ decodeUtf8 bs

data Chat = Chat
  { text :: T.Text
  } deriving (Show,Eq,Generic)

instance A.ToJSON Chat
instance A.FromJSON Chat

instance Serialize Chat where
  put = putNetcodeByteString . BL.toStrict . A.encode
  get = do
    bs <- getNetcodeByteString
    case A.eitherDecodeStrict bs of
      Left err -> fail err
      Right chat -> return chat

type VarInt = Int

putVarInt :: Int -> Put
putVarInt i
  | i < 0     = putVarInt (abs i + (2^31 :: Int))
  | i < 0x80  = putWord8 (fromIntegral i)
  | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> putVarInt (i `shiftR` 7)

getVarInt :: Get Int
getVarInt = do
    w <- getWord8
    if testBit w 7
      then do
        result <- go 7 (fromIntegral (w .&. 0x7F))
        if result <= (2^31)
          then return result
          else return (negate (result - (2^31)))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- getWord8
      if testBit w' 7
        then go (n+7) (val .|. ((fromIntegral (w' .&. 0x7F)) `shiftL` n))
        else return (val .|. ((fromIntegral w') `shiftL` n))

type VarLong = Int64

putVarLong :: Int64 -> Put
putVarLong i
  | i < 0     = putVarLong (abs i + (2^62 :: Int64))
  | i < 0x80  = putWord8 (fromIntegral i)
  | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >>  putVarLong (i `shiftR` 7)

getVarLong :: Get Int64
getVarLong = do
    w <- getWord8
    if testBit w 7
      then do
        result <- go 7 (fromIntegral (w .&. 0x7F))
        if result <= (2^62)
          then return result
          else return (negate (result - (2^62 :: Int64)))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- getWord8
      if testBit w' 7
        then go (n+7) (val .|. (fromIntegral (w' .&. 0x7F) `shiftL` n))
        else return (val .|. (fromIntegral w' `shiftL` n))

type EntityMetadata = V.Vector EntityMetadataEntry

putEntityMetadata :: EntityMetadata -> Put
putEntityMetadata e = do
  putVarInt . V.length $ e
  V.mapM_ put e

getEntityMetadata :: Get EntityMetadata
getEntityMetadata = do
  ln <- getVarInt
  V.replicateM ln get

data EntityMetadataEntry
  = MetadataEnd
  | Entry !Word8 !Int8 !MetadataType
  deriving (Show,Eq,Generic)

instance Serialize EntityMetadataEntry where
  put MetadataEnd = do
    putWord8 0xff
  put (Entry i t v) = do
    putWord8 i
    put t
    put v

  get = do
    i <- getWord8
    case i of
      0xff -> return MetadataEnd
      _ -> do
        t <- getInt8
        v <- get
        return $ Entry i t v

data MetadataType
  = MetadataByte
  | MetadataVarInt
  | MetadataFloat
  | MetadataString
  | MetadataChat
  | MetadataSlot
  | MetadataBool
  | MetadataRotation
  | MetadataPosition
  | MetadataOptPosition
  | MetadataDirection
  | MetadataOptUUID
  | MetadataBlockID
  deriving (Show,Eq,Enum,Generic)

instance Serialize MetadataType where
  put m = putWord8 (toEnum . fromEnum $ m)
  get = (toEnum . fromEnum) <$> getWord8

data Slot = Slot
  { blockID     :: !Int16
  , slotData    :: !(Maybe SlotData)
  } deriving (Show,Eq,Generic)

instance Serialize Slot where
  put (Slot bid sd) = do
    put bid
    case bid of
      (-1) -> return ()
      _ -> do
        let sd' = fromJust sd
        put sd'

  get = do
    blockID <- getInt16be
    case blockID of
      (-1) -> return $ Slot blockID Nothing
      _ -> do
        slotData <- get
        return $ Slot blockID (Just slotData)

data SlotData = SlotData
  { itemCount   :: !Int8
  , itemDamage  :: !Int16
  , nbt         :: !NBT
  } deriving (Show,Eq,Generic)

instance Serialize SlotData where
  put (SlotData itemCount' itemDamage' nbt') = do
    put itemCount'
    put itemDamage'
    put nbt'
  get = SlotData <$> get <*> get <*> get

mkSlot :: Int16 -> Int8 -> Int16 -> NBT -> Slot
mkSlot blockID itemCount itemDamage nbt =
  case blockID of
    -1 -> Slot blockID Nothing
    _ -> Slot blockID (Just (SlotData itemCount itemDamage nbt))

type Position = Word64

putPosition :: Position -> Put
putPosition = putWord64be

getPosition :: Get Position
getPosition = getWord64be

type Angle = Word8

putAngle :: Angle -> Put
putAngle = putWord8

getAngle :: Get Angle
getAngle = getWord8

putUUID :: UUID -> Put
putUUID uuid = putText (toText uuid)

getUUID :: Get UUID
getUUID = do
  txt <- getText
  case fromText txt of
    Just uuid -> return uuid
    Nothing -> fail "Error: Could not deserialize UUID!"

putUUID' :: UUID -> Put
putUUID' uuid = putByteString (BL.toStrict . toByteString $ uuid)

getUUID' :: Get UUID
getUUID' = do
  bs <- getBytes 16
  case (fromByteString . BL.fromStrict $ bs) of
    Just uuid -> return uuid
    Nothing -> fail "Error: Could not decode UUID!"

putNetcodeByteString :: B.ByteString -> Put
putNetcodeByteString b = do
  putVarInt . B.length $ b
  putByteString b

getNetcodeByteString :: Get B.ByteString
getNetcodeByteString = do
  ln <- getVarInt
  bs <- getBytes ln
  return bs

data Animation
  = SwingArm
  | TakeDamage
  | LeaveBed
  | EatFood
  | CriticalEffect
  | MagicCriticalEffect
  deriving (Show,Eq,Enum,Generic)

instance Serialize Animation where
  put = putWord8 . toEnum . fromEnum
  get = (toEnum . fromEnum) <$> getWord8

data BlockAction
  = NoteBlockAction !InstrumentType !NotePitch
  | PistonBlockAction !PistonState !PistonDirection
  | ChestBlockAction !Word8
  deriving (Show,Eq,Generic)

instance Serialize BlockAction where
  put (NoteBlockAction i n) = do
    putWord8 . toEnum . fromEnum $ i
    putWord8 . toEnum . fromEnum $ n
    putVarInt 25
  put (PistonBlockAction ps pd) = do
    putWord8 . toEnum . fromEnum $ ps
    putWord8 . toEnum . fromEnum $ pd
    putVarInt 33
  put (ChestBlockAction b) = do
    putWord8 1
    putWord8 b
    putVarInt 54

  get = do
    byte1 <- getWord8
    byte2 <- getWord8
    blockType <- getVarInt
    case blockType of
      25 -> return $ NoteBlockAction (toEnum . fromEnum $ byte1) (toEnum . fromEnum $ byte2)
      33 -> return $ PistonBlockAction (toEnum . fromEnum $ byte1) (toEnum . fromEnum $ byte2)
      54 -> return $ ChestBlockAction byte2
      err -> fail $ "Error: invalid BlockAction type: " ++ show err

data InstrumentType
  = Harp
  | DoubleBass
  | SnareDrum
  | Clicks
  | BassDrum
  deriving (Show,Eq,Enum,Generic)

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
  deriving (Show,Eq,Enum,Generic)

data PistonState
  = PistonPushing
  | PistonPulling
  deriving (Show,Eq,Enum,Generic)

data PistonDirection
  = PistonDown
  | PistonUp
  | PistonSouth
  | PistonWest
  | PistonNorth
  | PistonEast
  deriving (Show,Eq,Enum,Generic)

data BlockChange = BlockChange
  { hPosition     :: !Word8
  , yCoord        :: !Word8
  , blockId       :: !Int
  } deriving (Show,Eq,Generic)

instance Serialize BlockChange where
  put (BlockChange h y b) = do
    putWord8 h
    putWord8 y
    putVarInt b

  get = do
    h <- getWord8
    y <- getWord8
    b <- getVarInt
    return $ BlockChange h y b

data BossBarAction
  = BossBarAdd !Chat !Float !Int !Int !Word8
  | BossBarRemove
  | BossBarUpdateHealth !Float
  | BossBarUpdateTitle !Chat
  | BossBarUpdateStyle !Int !Int
  | BossBarUpdateFlags !Word8
  deriving (Show,Eq,Generic)

instance Serialize BossBarAction where
  put (BossBarAdd title health color division flags) = do
    putVarInt 0
    put title
    putFloat32be health
    putVarInt color
    putVarInt division
    putWord8 flags
  put BossBarRemove = do
    putVarInt 1
  put (BossBarUpdateHealth health) = do
    putVarInt 2
    putFloat32be health
  put (BossBarUpdateTitle title) = do
    putVarInt 3
    put title
  put (BossBarUpdateStyle color dividers) = do
    putVarInt 4
    putVarInt . toEnum $ color
    putVarInt . toEnum $ dividers
  put (BossBarUpdateFlags flags) = do
    putVarInt 5
    putWord8 flags

  get = do
    action <- getVarInt
    case action of
      0 -> do
        title <- get
        health <- getFloat32be
        color <- getVarInt
        division <- getVarInt
        flags <- getWord8
        return $ BossBarAdd title health color division flags
      1 -> return $ BossBarRemove
      2 -> do
        health <- getFloat32be
        return $ BossBarUpdateHealth health
      3 -> do
        title <- get
        return $ BossBarUpdateTitle title
      4 -> do
        color <- getVarInt
        dividers <- getVarInt
        return $ BossBarUpdateStyle color dividers
      5 -> do
        flags <- getWord8
        return $ BossBarUpdateFlags flags
      err -> fail $ "Error: Invalid BossBar action byte: " ++ show err

data Difficulty = Peaceful | Easy | Normal | Hard
  deriving (Show,Enum,Eq,Generic)

instance A.FromJSON Difficulty
instance A.ToJSON Difficulty
instance NFData Difficulty

instance Serialize Difficulty where
  put = putWord8 . toEnum . fromEnum
  get = (toEnum . fromEnum) <$> getWord8

data Dimension = Overworld | Nether | End
  deriving (Show,Eq,Generic)

instance Enum Dimension where
    fromEnum Overworld = 0
    fromEnum Nether = -1
    fromEnum End = 1
    toEnum 0 = Overworld
    toEnum (-1) = Nether
    toEnum 1 = End
    toEnum _ = undefined

instance A.ToJSON Dimension
instance A.FromJSON Dimension
instance NFData Dimension

instance Serialize Dimension where
  put d = put (toEnum . fromEnum $ d :: Int32)
  get = (toEnum . fromEnum) <$> getInt32be

data GameMode = Survival | Creative | Adventure | Spectator
  deriving (Show,Enum,Eq,Generic)

instance A.FromJSON GameMode
instance A.ToJSON GameMode
instance NFData GameMode

instance Serialize GameMode where
  put = putWord8 . toEnum . fromEnum
  get = (toEnum . fromEnum) <$> getWord8

data ProtocolState = ProtocolHandshake | ProtocolStatus | ProtocolLogin | ProtocolPlay
  deriving (Show,Eq,Enum,Generic)

instance Serialize ProtocolState where
  put = putWord8 . toEnum . fromEnum
  get = (toEnum . fromEnum) <$> getWord8

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
  deriving (Show,Eq,Generic)

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
  toEnum _ = undefined

instance Serialize UpdateBlockEntityAction where
  put ubea = putWord8 (toEnum . fromEnum $ ubea)
  get = (toEnum . fromEnum) <$> getWord8

data WindowProperty
  = WindowFurnace FurnaceProperty
  | WindowEnchantmentTable EnchantmentTableProperty
  | WindowBeacon BeaconProperty
  | WindowAnvil AnvilProperty
  | WindowBrewingStand BrewingStandProperty
  deriving (Show,Eq,Generic)

data FurnaceProperty
  = FireIcon
  | MaxBurnTime
  | ProgressArrow
  | MaxProgress
  deriving (Show,Eq,Enum,Generic)

data EnchantmentTableProperty
  = LvlReqTopSlot
  | LvlReqMiddleSlot
  | LvlReqBottomSlot
  | Seed
  | TopMouseHover
  | MiddleMouseHover
  | BottomMouseHover
  deriving (Show,Eq,Enum,Generic)

data BeaconProperty
  = PowerLevel
  | FirstPotionEffect
  | SecondPotionEffect
  deriving (Show,Eq,Enum,Generic)

data AnvilProperty
  = RepairCost
  deriving (Show,Eq,Enum,Generic)

data BrewingStandProperty
  = BrewTime
  deriving (Show,Eq,Enum,Generic)

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
  deriving (Show,Eq,Enum,Generic)

instance Serialize EntityStatus where
  put es = put (toEnum . fromEnum $ es :: Int8)
  get = (toEnum . fromEnum) <$> getInt8

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
  deriving (Show,Eq,Generic)

instance Enum GameChangeReason where
  fromEnum InvalidBed = 0
  fromEnum EndRaining = 1
  fromEnum BeginRaining = 2
  fromEnum ChangeGameMode = 3
  fromEnum ExitEnd = 4
  fromEnum DemoMessage = 5
  fromEnum ArrowHittingPlayer = 6
  fromEnum FadeValue = 7
  fromEnum FateTime = 8
  fromEnum PlayElderGuardianAppearance = 10

  toEnum 0 = InvalidBed
  toEnum 1 = EndRaining
  toEnum 2 = BeginRaining
  toEnum 3 = ChangeGameMode
  toEnum 4 = ExitEnd
  toEnum 5 = DemoMessage
  toEnum 6 = ArrowHittingPlayer
  toEnum 7 = FadeValue
  toEnum 8 = FateTime
  toEnum 10 = PlayElderGuardianAppearance
  toEnum _ = undefined

instance Serialize GameChangeReason where
  put gcr = putWord8 (toEnum . fromEnum $ gcr)
  get = (toEnum . fromEnum) <$> getWord8

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
  deriving (Show,Eq,Generic)

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
  deriving (Show,Eq,Enum,Generic)

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
  deriving (Show,Eq,Enum,Generic)

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
  deriving (Show,Eq,Enum,Generic)

data StatusPayload = StatusPayload
  { version       :: !Version
  , players       :: !Players
  , description   :: !Description
  } deriving (Generic,Show,Eq,Read)

instance A.ToJSON StatusPayload
instance A.FromJSON StatusPayload

data Version = Version
  { name      :: !T.Text
  , protocol  :: !Word8
  } deriving (Generic,Eq,Show,Read)

instance A.ToJSON Version
instance A.FromJSON Version

data Players = Players
  { max     :: !Word8
  , online  :: !Word8
  } deriving (Generic,Eq,Show,Read)

instance A.ToJSON Players
instance A.FromJSON Players

data Description = Description
  { text    :: !T.Text
  } deriving (Generic,Eq,Show,Read)

instance A.ToJSON Description
instance A.FromJSON Description

data Statistic = Statistic !T.Text !VarInt deriving (Show,Eq,Generic)

instance Serialize Statistic where
  put (Statistic t val) = do
    putText t
    putVarInt val
  get = do
    t <- getText
    v <- getVarInt
    return $ Statistic t v

data PlayerListEntries
  = PlayerListAdds (V.Vector PlayerListAdd)
  | PlayerListUpdateGameModes (V.Vector PlayerListUpdateGameMode)
  | PlayerListUpdateLatencies (V.Vector PlayerListUpdateLatency)
  | PlayerListUpdateDisplayNames (V.Vector PlayerListUpdateDisplayName)
  | PlayerListRemovePlayers (V.Vector PlayerListRemovePlayer)
  deriving (Show,Eq,Generic)

instance Serialize PlayerListEntries where
  put (PlayerListAdds lst) = do
    putVarInt 0
    putVarInt . V.length $ lst
    V.mapM_ put lst
  put (PlayerListUpdateGameModes lst) = do
    putVarInt 1
    putVarInt . V.length $ lst
    V.mapM_ put lst
  put (PlayerListUpdateLatencies lst) = do
    putVarInt 2
    putVarInt . V.length $ lst
    V.mapM_ put lst
  put (PlayerListUpdateDisplayNames lst) = do
    putVarInt 3
    putVarInt . V.length $ lst
    V.mapM_ put lst
  put (PlayerListRemovePlayers lst) = do
    putVarInt 4
    putVarInt . V.length $ lst
    V.mapM_ put lst

  get = do
    action <- getVarInt
    ln <- getVarInt
    case action of
      0 -> PlayerListAdds <$> V.replicateM ln get
      1 -> PlayerListUpdateGameModes <$> V.replicateM ln get
      2 -> PlayerListUpdateLatencies <$> V.replicateM ln get
      3 -> PlayerListUpdateDisplayNames <$> V.replicateM ln get
      4 -> PlayerListRemovePlayers <$> V.replicateM ln get
      err -> fail $ "Error: Invaid PlayerList action " ++ show err

data PlayerListAdd = PlayerListAdd !UUID !T.Text !(V.Vector PlayerProperty) !GameMode !Int !(Maybe T.Text)
  deriving (Show,Eq,Generic)

instance Serialize PlayerListAdd where
  put (PlayerListAdd uuid name properties gamemode ping maybeDisplayName) = do
    putUUID' uuid
    putText name
    putVarInt . V.length $ properties
    V.mapM_ put properties
    putVarInt (fromEnum gamemode)
    putVarInt ping
    case maybeDisplayName of
      Just displayName -> do
        put True
        putText displayName
      Nothing -> do
        put False

  get = do
    uuid <- getUUID'
    name <- getText
    count <- getVarInt
    properties <- V.replicateM count get
    gameMode <- toEnum <$> getVarInt
    ping <- getVarInt
    hasDisplayName <- get
    if hasDisplayName
      then do
        displayName <- getText
        return $ PlayerListAdd uuid name properties gameMode ping (Just displayName)
      else
        return $ PlayerListAdd uuid name properties gameMode ping Nothing

data PlayerListUpdateGameMode = PlayerListUpdateGameMode !UUID !GameMode
  deriving (Show,Eq,Generic)

instance Serialize PlayerListUpdateGameMode where
  put (PlayerListUpdateGameMode uuid gameMode) = do
    putUUID' uuid
    putVarInt . fromEnum $ gameMode

  get = do
    uuid <- getUUID'
    gameMode <- toEnum <$> getVarInt
    return $ PlayerListUpdateGameMode uuid gameMode

data PlayerListUpdateLatency = PlayerListUpdateLatency !UUID !Int
  deriving (Show,Eq,Generic)

instance Serialize PlayerListUpdateLatency where
  put (PlayerListUpdateLatency uuid ping) = do
    putUUID' uuid
    putVarInt ping

  get = do
    uuid <- getUUID'
    ping <- getVarInt
    return $ PlayerListUpdateLatency uuid ping

data PlayerListUpdateDisplayName = PlayerListUpdateDisplayName !UUID !(Maybe T.Text)
  deriving (Show,Eq,Generic)

instance Serialize PlayerListUpdateDisplayName where
  put (PlayerListUpdateDisplayName uuid maybeDisplayName) = do
    putUUID' uuid
    case maybeDisplayName of
      Nothing -> put False
      Just displayName -> do
        put True
        putText displayName

  get = do
    uuid <- getUUID'
    hasDisplayName <- get
    if hasDisplayName
      then do
        displayName <- getText
        return $ PlayerListUpdateDisplayName uuid (Just displayName)
      else
        return $ PlayerListUpdateDisplayName uuid Nothing

data PlayerListRemovePlayer = PlayerListRemovePlayer !UUID
  deriving (Show,Eq,Generic)

instance Serialize PlayerListRemovePlayer where
  put (PlayerListRemovePlayer uuid) = putUUID' uuid
  get = PlayerListRemovePlayer <$> getUUID'

data PlayerProperty = PlayerProperty
  { playerName    :: !T.Text
  , playerValue   :: !T.Text
  , playerSig     :: !(Maybe T.Text)
  } deriving (Show,Eq,Generic)

instance Serialize PlayerProperty where
  put (PlayerProperty name val maybeSig) = do
    putText name
    putText val
    case maybeSig of
      Nothing -> put False
      Just sig -> do
        put True
        putText sig

  get = do
    name <- getText
    value <- getText
    isSigned <- get
    if isSigned
      then do
        sig <- getText
        return $ PlayerProperty name value (Just sig)
      else
        return $ PlayerProperty name value Nothing

data Icon = Icon
  { directionAndType  :: !Word8
  , x                 :: !Word8
  , z                 :: !Word8
  } deriving (Show,Eq,Generic)

instance Serialize Icon where
  put (Icon directionAndType' x' z') = do
    putWord8 directionAndType'
    putWord8 x'
    putWord8 z'

  get = Icon <$> getWord8 <*> getWord8 <*> getWord8

data CombatEvent
  = EnterCombat
  | EndCombat !Int !Int32
  | EntityDead !Int !Int32 !Chat
  deriving (Show,Eq,Generic)

instance Serialize CombatEvent where
  put EnterCombat = do
    putVarInt 0
  put (EndCombat duration entityID) = do
    putVarInt 1
    putVarInt duration
    put entityID
  put (EntityDead playerID entityID message) = do
    putVarInt 2
    putVarInt playerID
    put entityID
    put message

  get = do
    event <- getVarInt
    case event of
      0 -> return EnterCombat
      1 -> do
        duration <- getVarInt
        entityID <- getInt32be
        return $ EndCombat duration entityID
      2 -> do
        playerID <- getVarInt
        entityID <- getInt32be
        message <- get
        return $ EntityDead playerID entityID message
      err -> fail $ "Error: Unrecognized combat event: " ++ show err

data WorldBorderAction
  = SetSize !Double
  | LerpSize !Double !Double !Int64
  | SetCenter !Double !Double
  | Initialize !Double !Double !Double !Double !Int64 !Int !Int !Int
  | SetWarningTime !Int
  | SetWarningBlocks !Int
  deriving (Show,Eq,Generic)

instance Serialize WorldBorderAction where
  put (SetSize diameter) = do
    putVarInt 0
    putFloat64be diameter
  put (LerpSize oldDiameter newDiameter speed) = do
    putVarInt 1
    putFloat64be oldDiameter
    putFloat64be newDiameter
    putVarLong speed
  put (SetCenter x z) = do
    putVarInt 2
    putFloat64be x
    putFloat64be z
  put (Initialize x z oldDiameter newDiameter speed portalBoundary warningTime warningBlocks) = do
    putVarInt 3
    putFloat64be x
    putFloat64be z
    putFloat64be oldDiameter
    putFloat64be newDiameter
    putVarLong speed
    putVarInt portalBoundary
    putVarInt warningTime
    putVarInt warningBlocks
  put (SetWarningTime warningTime) = do
    putVarInt 4
    putVarInt . toEnum $ warningTime
  put (SetWarningBlocks warningBlocks) = do
    putVarInt 5
    putVarInt . toEnum $ warningBlocks

  get = do
    action <- getVarInt
    case action of
      0 -> do
        diameter <- getFloat64be
        return $ SetSize diameter
      1 -> do
        oldDiameter <- getFloat64be
        newDiameter <- getFloat64be
        speed <- getVarLong
        return $ LerpSize oldDiameter newDiameter speed
      2 -> do
        x <- getFloat64be
        z <- getFloat64be
        return $ SetCenter x z
      3 -> do
        x <- getFloat64be
        z <- getFloat64be
        oldDiameter <- getFloat64be
        newDiameter <- getFloat64be
        speed <- getVarLong
        portalBoundary <- getVarInt
        warningTime <- getVarInt
        warningBlocks <- getVarInt
        return $ Initialize x z oldDiameter newDiameter speed portalBoundary warningTime warningBlocks
      4 -> do
        warningTime <- getVarInt
        return $ SetWarningTime warningTime
      5 -> do
        warningBlocks <- getVarInt
        return $ SetWarningBlocks warningBlocks
      err -> fail $ "Error: Unrecognized world border action: " ++ show err

data TeamMode
  = CreateTeam !T.Text !T.Text !T.Text !Int8 !T.Text !T.Text !Int8 !(V.Vector T.Text)
  | RemoveTeam
  | UpdateTeamInfo !T.Text !T.Text !T.Text !Int8 !T.Text !T.Text !Int8
  | AddPlayers !(V.Vector T.Text)
  | RemovePlayers !(V.Vector T.Text)
  deriving (Show,Eq,Generic)

instance Serialize TeamMode where
  put (CreateTeam displayName prefix suffix flags tagVisibility collision color players) = do
    put (0 :: Int8)
    putText displayName
    putText prefix
    putText suffix
    put flags
    putText tagVisibility
    putText collision
    put color
    putVarInt . V.length $ players
    mapM_ putText players
  put RemoveTeam = put (1 :: Int8)
  put (UpdateTeamInfo displayName prefix suffix flags tagVisibility collision color) = do
    put (2 :: Int8)
    putText displayName
    putText prefix
    putText suffix
    put flags
    putText tagVisibility
    putText collision
    put color
  put (AddPlayers players) = do
    put (3 :: Int8)
    putVarInt . V.length $ players
    mapM_ putText players
  put (RemovePlayers players) = do
    put (4 :: Int8)
    putVarInt . V.length $ players
    mapM_ putText players

  get = do
    mode <- getInt8
    case mode of
      0 -> do
        displayName <- getText
        prefix <- getText
        suffix <- getText
        flags <- getInt8
        tagVisibility <- getText
        collision <- getText
        color <- getInt8
        count <- getVarInt
        players <- V.replicateM count getText
        return $ CreateTeam displayName prefix suffix flags tagVisibility collision color players
      1 -> return RemoveTeam
      2 -> do
        displayName <- getText
        prefix <- getText
        suffix <- getText
        flags <- getInt8
        tagVisibility <- getText
        collision <- getText
        color <- getInt8
        return $ UpdateTeamInfo displayName prefix suffix flags tagVisibility collision color
      3 -> do
        count <- getVarInt
        players <- V.replicateM count getText
        return $ AddPlayers players
      4 -> do
        count <- getVarInt
        players <- V.replicateM count getText
        return $ RemovePlayers players
      err -> fail $ "Unrecognized team mode: " ++ show err

data TitleAction
  = SetTitle !Chat
  | SetSubtitle !Chat
  | SetTimesAndDisplay !Int32 !Int32 !Int32
  | Hide
  | Reset
  deriving (Show,Eq,Generic)

instance Serialize TitleAction where
  put (SetTitle titleText) = do
    putVarInt 0
    put titleText
  put (SetSubtitle subtitleText) = do
    putVarInt 1
    put subtitleText
  put (SetTimesAndDisplay fadeIn stay fadeOut) = do
    putVarInt 2
    put fadeIn
    put stay
    put fadeOut
  put Hide = putVarInt 3
  put Reset = putVarInt 4

  get = do
    action <- getVarInt
    case action of
      0 -> do
        titleText <- get
        return $ SetTitle titleText
      1 -> do
        subtitleText <- get
        return $ SetSubtitle subtitleText
      2 -> do
        fadeIn <- getInt32be
        stay <- getInt32be
        fadeOut <- getInt32be
        return $ SetTimesAndDisplay fadeIn stay fadeOut
      3 -> return Hide
      4 -> return Reset
      err -> fail $ "Unrecognized title action: " ++ show err

data EntityProperty = EntityProperty
  { key             :: !T.Text
  , value           :: !Double
  , numOfModifiers  :: !Int
  , modifiers       :: !Int
  } deriving (Show,Eq,Generic)

instance Serialize EntityProperty where
  put (EntityProperty k v n m) = do
    putText k
    putFloat64be v
    putVarInt n
    putVarInt m

  get = do
    k <- getText
    v <- getFloat64be
    n <- getVarInt
    m <- getVarInt
    return $ EntityProperty k v n m

encodeStatusPayload :: T.Text -> Word8 -> Word8 -> Word8 -> T.Text -> Put
encodeStatusPayload mcVersion versionID currentPlayers maxPlayers motd =
  putNetcodeByteString . BL.toStrict . A.encode $
    StatusPayload (Version mcVersion versionID)
                  (Players maxPlayers currentPlayers)
                  (Description motd)

data UpdatedColumns
  = NoUpdatedColumns
  | UpdatedColumns !Int8 !Int8 !Int8 !Int8 !B.ByteString
  deriving (Show,Eq,Generic)

instance Serialize UpdatedColumns where
  put NoUpdatedColumns = do
    put (0 :: Int8)
  put (UpdatedColumns col rows x z dat) = do
    put col
    put rows
    put x
    put z
    putNetcodeByteString dat

  get = do
    columns <- getInt8
    if columns > 0
      then do
        rows <- getInt8
        x <- getInt8
        z <- getInt8
        dat <- getNetcodeByteString
        return $ UpdatedColumns columns rows x z dat
      else return NoUpdatedColumns

data UpdateScoreAction
  = CreateOrUpdateScoreItem !T.Text !T.Text !VarInt
  | RemoveScoreItem !T.Text !T.Text
  deriving (Show,Eq,Generic)

instance Serialize UpdateScoreAction where
  put (CreateOrUpdateScoreItem scoreName objectiveName val) = do
    putText scoreName
    put (0 :: Int8)
    putText objectiveName
    putVarInt val
  put (RemoveScoreItem scoreName objectiveName) = do
    putText scoreName
    put (1 :: Int8)
    putText objectiveName

  get = do
    scoreName <- getText
    action <- getInt8
    objectiveName <- getText
    case action of
      0 -> do
        value <- getVarInt
        return $ CreateOrUpdateScoreItem scoreName objectiveName value
      1 -> return $ RemoveScoreItem scoreName objectiveName
      err -> fail $ "Error: Invalid UpdateScoreAction byte: " ++ show err

data UseEntityType
  = InteractWithEntity !VarInt
  | AttackEntity
  | InteractAtEntity !Float !Float !Float !EntityHand
  deriving (Show,Eq,Generic)

instance Serialize UseEntityType where
  put (InteractWithEntity h) = do
    putVarInt 0
    putVarInt h
  put AttackEntity = putVarInt 1
  put (InteractAtEntity tX tY tZ h) = do
    putVarInt 2
    putFloat32be tX
    putFloat32be tY
    putFloat32be tZ
    putVarInt (fromEnum h)

  get = do
    t <- getVarInt
    case t of
      0 -> InteractWithEntity <$> getVarInt
      1 -> return AttackEntity
      2 -> InteractAtEntity <$> getFloat32be <*> getFloat32be <*> getFloat32be <*> get
      err -> fail $ "Error: Could not get UseEntityType type: " ++ show err

data EntityHand = MainHand | OffHand deriving (Show,Eq,Enum,Generic)

instance Serialize EntityHand where
  put = putWord8 . toEnum . fromEnum
  get = (toEnum . fromEnum) <$> getWord8

data ScoreboardMode
  = CreateScoreboard !T.Text !T.Text
  | RemoveScoreboard
  | UpdateDisplayText !T.Text !T.Text
  deriving (Show,Eq,Generic)

instance Serialize ScoreboardMode where
  put (CreateScoreboard ov t) = do
    put (0 :: Int8)
    putText ov
    putText t
  put RemoveScoreboard = put (1 :: Int8)
  put (UpdateDisplayText ov t) = do
    put (2 :: Int8)
    putText ov
    putText t

  get = do
    mode <- getInt8
    case mode of
      0 -> do
        objectiveValue <- getText
        t <- getText
        return $ CreateScoreboard objectiveValue t
      1 -> return RemoveScoreboard
      2 -> do
        objectiveValue <- getText
        t <- getText
        return $ UpdateDisplayText objectiveValue t
      err -> fail $ "Error: Invalid ScoreboardMode byte: " ++ show err
