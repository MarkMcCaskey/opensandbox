{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data.Block
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data.Block
  ( Block (..)
  , BlockID (..)
  , Half (..)
  , Axis (..)
  , Facing4 (..)
  , Facing5 (..)
  , Facing6 (..)
  , AnvilDamage (..)
  , BannerStanding (..)
  , BedPart (..)
  , BeetrootAge (..)
  , CactusAge (..)
  , CakeBites (..)
  , Color (..)
  , CarrotAge (..)
  , CauldronLevel (..)
  , ChorusFlowerAge (..)
  , CobblestoneWallVariant (..)
  , CocoaAge (..)
  , DaylightSensorPower (..)
  , DirtVariant (..)
  , DoorHalf (..)
  , DoorHinge (..)
  , FarmlandMoisture (..)
  , FireAge (..)
  , RedFlowersType (..)
  , YellowFlowersType (..)
  , FlowerPotContents (..)
  , FrostedIceAge (..)
  , GrassType (..)
  , HopperFacing (..)
  , FluidLevel (..)
  , LeavesVariant (..)
  , Leaves2Variant (..)
  , LeverFacing (..)
  , MelonStemAge (..)
  , MonsterEggVariant (..)
  , MushroomVariant (..)
  , NetherWartAge (..)
  , PistonHeadType (..)
  , NetherPortalAxis (..)
  , PotatoAge (..)
  , PrismarineVariant (..)
  , PumpkinStemAge (..)
  , QuartzVariant (..)
  , RailShape (..)
  , UtilityRailShape (..)
  , RedSandstoneType (..)
  , ComparatorMode (..)
  , RedstonePower (..)
  , RedstoneRepeaterDelay (..)
  , SandVariant (..)
  , SandstoneType (..)
  , SaplingType (..)
  , SignStandingRotation
  , StoneSlabVariant (..)
  , StoneSlab2Variant (..)
  , WoodenSlabVariant (..)
  , PurpurSlabVariant (..)
  , DoubleStoneSlabVariant (..)
  , DoubleWoodenSlabVariant (..)
  , PurpurDoubleSlabVariant (..)
  , SnowLayers (..)
  , StairShape (..)
  , StoneVariant (..)
  , StoneBrickVariant (..)
  , StructureBlockMode (..)
  , SugarCaneAge (..)
  , DoublePlantVariant (..)
  , WeightedPressurePlatePower (..)
  , WheatAge (..)
  , LogAxis (..)
  , LogVariant (..)
  , Log2Variant (..)
  , WoodPlanksVariant (..)
  , RedSandstoneSlabs (..)
  ) where

import            Data.Aeson
import            Data.Aeson.Types
import qualified  Data.Attoparsec.Text as A
import            Data.Data
import qualified  Data.HashMap.Strict as H
import            Data.Scientific
import            Data.Text as T
import            Data.Word
import            Control.DeepSeq
import            GHC.Generics (Generic)
import            Prelude hiding (id)

data Block = Block
  { id            :: Word16
  , displayName   :: Text
  , name          :: Text
  , hardness      :: Double
  , stackSize     :: Word8
  , diggable      :: Bool
  , material      :: Maybe Material
  , harvestTools  :: Maybe [Word32]
  , variations    :: Maybe [Variation]
  , drops         :: [Drop]
  , transparent   :: Bool
  , emitLight     :: Word8
  , filterLight   :: Word8
  } deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON Block where
  parseJSON (Object v) = Block
      <$> v .: "id"
      <*> v .: "displayName"
      <*> v .: "name"
      <*> v .: "hardness"
      <*> v .: "stackSize"
      <*> v .: "diggable"
      <*> v .:? "material"
      <*> (fmap . fmap) extractIds (v .:? "harvestTools" :: Parser (Maybe Object))
      <*> v .:? "variations"
      <*> v .: "drops"
      <*> v .: "transparent"
      <*> v .: "emitLight"
      <*> v .: "filterLight"
    where
    extractIds obj = case sequence (fmap extractId (H.keys obj)) of
                      Left err -> fail err
                      Right lst -> lst
    extractId x = A.parseOnly A.decimal x :: Either String Word32
  parseJSON x = typeMismatch "Error: Invalid Block!" x

instance NFData Block

data Material
  = MaterialRock
  | MaterialDirt
  | MaterialWood
  | MaterialPlant
  | MaterialLeaves
  | MaterialWeb
  | MaterialWool
  deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON Material where
  parseJSON (String s) =
    case s of
      "rock" -> return MaterialRock
      "dirt" -> return MaterialDirt
      "wood" -> return MaterialWood
      "plant" -> return MaterialPlant
      "leaves" -> return MaterialLeaves
      "web" -> return MaterialWeb
      "wool" -> return MaterialWool
      x       -> fail $ "ERROR => Aeson => failed to pattern match text to Material: " ++ show x

  parseJSON x = typeMismatch ("ERROR => Aeson => Material => not a String, got " ++ show x) x

instance NFData Material

data Drop = Drop
  { drop      :: DropEntry
  , minCount  :: Maybe Word8
  , maxCount  :: Maybe Word8
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance FromJSON Drop
instance NFData Drop

newtype DropEntry = DropEntry (Either Word32 DropBody)
  deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance FromJSON DropEntry where
  parseJSON (Number n) = return $ DropEntry . Left . toEnum . base10Exponent $ n
  parseJSON (Object o) = fmap (DropEntry . Right) $ DropBody
    <$> o .: "id"
    <*> o .: "metadata"
  parseJSON x = typeMismatch "Error: Invalid DropEntry!" x

instance NFData DropEntry

data DropBody = DropBody
  { id        :: Word32
  , metadata  :: Word32
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance FromJSON DropBody
instance NFData DropBody

data Variation = Variation
  { metadata      :: Word8
  , displayName   :: Text
  } deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance ToJSON Variation
instance FromJSON Variation
instance NFData Variation

data BlockID
  = Air
  | Stone
  | Grass
  | Dirt
  | Cobblestone
  | WoodPlanks
  | Sapling
  | Bedrock
  | FlowingWater
  | Water
  | FlowingLava
  | Lava
  | Sand
  | Gravel
  | GoldOre
  | IronOre
  | CoalOre
  | Log
  | Leaves
  | Sponge
  | Glass
  | LapisOre
  | LapisBlock
  | Dispenser
  | Sandstone
  | Noteblock
  | Bed
  | GoldenRail
  | DetectorRail
  | StickyPiston
  | Web
  | Tallgrass
  | Deadbush
  | Piston
  | PistonHead
  | Wool
  | PistonExtension
  | YellowFlower
  | RedFlower
  | BrownMushroom
  | RedMushroom
  | GoldBlock
  | IronBlock
  | DoubleStoneSlab
  | StoneSlab
  | BrickBlock
  | TNT
  | Bookshelf
  | MossyCobblestone
  | Obsidian
  | Torch
  | Fire
  | MobSpawner
  | OakStairs
  | Chest
  | RedstoneWire
  | DiamondOre
  | DiamondBlock
  | CraftingTable
  | Wheat
  | Farmland
  | Furnace
  | LitFurnace
  | StandingSign
  | WoodenDoor
  | Ladder
  | Rail
  | StoneStairs
  | WallSign
  | Lever
  | StonePressurePlate
  | IronDoor
  | WoodenPressurePlate
  | RedstoneOre
  | LitRedstoneOre
  | UnlitRedstoneTorch
  | RedstoneTorch
  | StoneButton
  | SnowLayer
  | Ice
  | Snow
  | Cactus
  | Clay
  | Reeds
  | Jukebox
  | Fence
  | Pumpkin
  | Netherrack
  | SoulSand
  | Glowstone
  | Portal
  | LitPumpkin
  | Cake
  | UnpoweredRepeater
  | PoweredRepeater
  | StainedGlass
  | Trapdoor
  | MonsterEgg
  | StoneBrick
  | BrownMushroomBlock
  | RedMushroomBlock
  | IronBars
  | GlassPane
  | MelonBlock
  | PumpkinStem
  | MelonStem
  | Vine
  | FenceGate
  | BrickStairs
  | StoneBrickStairs
  | Mycelium
  | Waterlily
  | NetherBrick
  | NetherBrickFence
  | NetherBrickStairs
  | NetherWart
  | EnchantingTable
  | BrewingStand
  | Cauldron
  | EndPortal
  | EndPortalFrame
  | EndStone
  | DragonEgg
  | RedstoneLamp
  | LitRedstoneLamp
  | DoubleWoodenSlab
  | WoodenSlab
  | Cocoa
  | SandstoneStairs
  | EmeraldOre
  | EnderChest
  | TripwireHook
  | Tripwire
  | EmeraldBlock
  | SpruceStairs
  | BirchStairs
  | JungleStairs
  | CommandBlock
  | Beacon
  | CobblestoneWall
  | FlowerPot
  | Carrots
  | Potatoes
  | WoodenButton
  | Skull
  | Anvil
  | TrappedChest
  | LightWeightedPressurePlate
  | HeavyWeightedPressurePlate
  | UnpoweredComparator
  | PoweredComparator
  | DaylightDetector
  | RedstoneBlock
  | QuartzOre
  | Hopper
  | QuartzBlock
  | QuartzStairs
  | ActivatorRail
  | Dropper
  | StainedHardenedClay
  | StainedGlassPane
  | Leaves2
  | Log2
  | AcaciaStairs
  | DarkOakStairs
  | Slime
  | Barrier
  | IronTrapdoor
  | Prismarine
  | SeaLantern
  | HayBlock
  | Carpet
  | HardenedClay
  | CoalBlock
  | PackedIce
  | DoublePlant
  | StandingBanner
  | WallBanner
  | DaylightDetectorInverted
  | RedSandstone
  | RedSandstoneStairs
  | DoubleStoneSlab2
  | StoneSlab2
  | SpruceFenceGate
  | BirchFenceGate
  | JungleFenceGate
  | DarkOakFenceGate
  | AcaciaFenceGate
  | SpruceFence
  | BirchFence
  | JungleFence
  | DarkOakFence
  | AcaciaFence
  | SpruceDoor
  | BirchDoor
  | JungleDoor
  | AcaciaDoor
  | DarkOakDoor
  | EndRod
  | ChorusPlant
  | ChorusFlower
  | PurpurBlock
  | PurpurPillar
  | PurpurStairs
  | PurpurDoubleSlab
  | PurpurSlab
  | EndBricks
  | Beetroots
  | GrassPath
  | EndGateway
  | RepeatingCommandBlock
  | ChainCommandBlock
  | FrostedIce
  | Magma
  | NetherWartBlock
  | RedNetherBrick
  | BoneBlock
  | StructureVoid
  -- | BlockIDStructureBlock
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData BlockID

type Snowy = Bool

data Half = Top | Bottom
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData Half

data Axis
  = AxisX
  | AxisY
  | AxisZ
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData Axis

data Facing4
  = Facing4N
  | Facing4S
  | Facing4E
  | Facing4W
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData Facing4

data Facing5
  = Facing5N
  | Facing5S
  | Facing5E
  | Facing5W
  | Facing5Up
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData Facing5

data Facing6
  = Facing6N
  | Facing6S
  | Facing6E
  | Facing6W
  | Facing6Up
  | Facing6Down
  deriving (Show,Eq,Ord,Generic,Data,Typeable)

instance NFData Facing6

data AnvilDamage
  = AnvilNoDamage
  | AnvilSlightlyDamaged
  | AnvilVeryDamaged
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData AnvilDamage

data BannerStanding
  = RotationS
  | RotationSSW
  | RotationSW
  | RotationWSW
  | RotationW
  | RotationWNW
  | RotationNW
  | RotationNNW
  | RotationN
  | RotationNNE
  | RotationNE
  | RotationENE
  | RotationE
  | RotationESE
  | RotationSE
  | RotationSSE
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData BannerStanding

type Occupied = Bool

data BedPart = Head | Foot
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData BedPart

data BeetrootAge
  = BeetrootAgeI
  | BeetrootAgeII
  | BeetrootAgeIII
  | BeetrootAgeIV
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData BeetrootAge

type HasBottle = Bool

type ButtonPowered = Bool

data CactusAge
  = CactusAge0
  | CactusAge1
  | CactusAge2
  | CactusAge3
  | CactusAge4
  | CactusAge5
  | CactusAge6
  | CactusAge7
  | CactusAge8
  | CactusAge9
  | CactusAge10
  | CactusAge11
  | CactusAge12
  | CactusAge13
  | CactusAge14
  | CactusAge15
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData CactusAge

data CakeBites
  = CakeBites0
  | CakeBites1
  | CakeBites2
  | CakeBites3
  | CakeBites4
  | CakeBites5
  | CakeBites6
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData CakeBites

data Color
  = White
  | Orange
  | Magenta
  | LightBlue
  | Yellow
  | Lime
  | Pink
  | Gray
  | LightGray
  | Cyan
  | Purple
  | Blue
  | Brown
  | Green
  | Red
  | Black
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData Color

data CarrotAge
  = CarrotAge0
  | CarrotAge1
  | CarrotAge2
  | CarrotAge3
  | CarrotAge4
  | CarrotAge5
  | CarrotAge6
  | CarrotAge7
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData CarrotAge

data CauldronLevel
  = CauldronLevel0
  | CauldronLevel1
  | CauldronLevel2
  | CauldronLevel3
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData CauldronLevel

data ChorusFlowerAge
  = ChorusFlowerAge0
  | ChorusFlowerAge1
  | ChorusFlowerAge2
  | ChorusFlowerAge3
  | ChorusFlowerAge4
  | ChorusFlowerAge5
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData ChorusFlowerAge

data CobblestoneWallVariant
  = PlainCobblestoneWall
  | MossyCobblestoneWall
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData CobblestoneWallVariant

data CocoaAge
  = CocoaAge0
  | CocoaAge1
  | CocoaAge2
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData CocoaAge

data DaylightSensorPower
  = DS_Power0
  | DS_Power1
  | DS_Power2
  | DS_Power3
  | DS_Power4
  | DS_Power5
  | DS_Power6
  | DS_Power7
  | DS_Power8
  | DS_Power9
  | DS_Power10
  | DS_Power11
  | DS_Power12
  | DS_Power13
  | DS_Power14
  | DS_Power15
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData DaylightSensorPower

data DirtVariant
  = DirtPlain
  | DirtCoarse
  | DirtPodzol
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData DirtVariant

data DoorHalf = DoorUpper | DoorLower
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData DoorHalf

data DoorHinge = HingeLeft | HingeRight
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData DoorHinge

data FarmlandMoisture
  = Moisture0
  | Moisture1
  | Moisture2
  | Moisture3
  | Moisture4
  | Moisture5
  | Moisture6
  | Moisture7
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData FarmlandMoisture

data FireAge
  = FireAge0
  | FireAge1
  | FireAge2
  | FireAge3
  | FireAge4
  | FireAge5
  | FireAge6
  | FireAge7
  | FireAge8
  | FireAge9
  | FireAge10
  | FireAge11
  | FireAge12
  | FireAge13
  | FireAge14
  | FireAge15
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData FireAge

data RedFlowersType
  = FlowerPoppy
  | FlowerBlueOrchid
  | FlowerAllium
  | FlowerHoustonia
  | FlowerRedTulip
  | FlowerOrangeTulip
  | FlowerWhiteTulip
  | FlowerPinkTulip
  | FlowerOxeyeDaisy
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData RedFlowersType

data YellowFlowersType = FlowerDandelion
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData YellowFlowersType

data FlowerPotContents
  = FPCEmpty
  | FPCRose
  | FPCBlueOrchid
  | FPCAllium
  | FPCHoustonia
  | FPCRedTulip
  | FPCOrangeTulip
  | FPCWhiteTulip
  | FPCPinkTulip
  | FPCOxeyeDaisy
  | FPCDandelion
  | FPCOakSapling
  | FPCSpruceSapling
  | FPCBirchSapling
  | FPCJungleSapling
  | FPCAcaciaSapling
  | FPCDarkOakSapling
  | FPCMushroomRed
  | FPCMushroomBrown
  | FPCDeadBush
  | FPCern
  | FPCCactus
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData FlowerPotContents

data FrostedIceAge
  = FrostedIceAge0
  | FrostedIceAge1
  | FrostedIceAge2
  | FrostedIceAge3
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData FrostedIceAge

data GrassType
  = GrassDeadBush
  | GrassTallGrass
  | GrassFern
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData GrassType

data HopperFacing
  = HopperFacingN
  | HopperFacingS
  | HopperFacingE
  | HopperFacingW
  | HopperFacingDown
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData HopperFacing

data FluidLevel
  = FluidLvl0
  | FluidLvl1
  | FluidLvl2
  | FluidLvl3
  | FluidLvl4
  | FluidLvl5
  | FluidLvl6
  | FluidLvl7
  | FluidLvl8
  | FluidLvl9
  | FluidLvl10
  | FluidLvl11
  | FluidLvl12
  | FluidLvl13
  | FluidLvl14
  | FluidLvl15
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData FluidLevel

data LeavesVariant
  = LeavesOak
  | LeavesSpruce
  | LeavesBirch
  | LeavesJungle
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData LeavesVariant

data Leaves2Variant
  = Leaves2Acacia
  | Leaves2DarkOak
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData Leaves2Variant

data LeverFacing
  = LeverBottomFacingE
  | LeverFacingSideN
  | LeverFacingSideS
  | LeverFacingSideE
  | LeverFacingSideW
  | LeverFacingUpE
  | LeverFacingDownE
  | LeverFacingUpS
  | LeverFacingDownS
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData LeverFacing

data MelonStemAge
  = MelonStemAge0
  | MelonStemAge1
  | MelonStemAge2
  | MelonStemAge3
  | MelonStemAge4
  | MelonStemAge5
  | MelonStemAge6
  | MelonStemAge7
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData MelonStemAge

data MonsterEggVariant
  = MonsterEggVariantStone
  | MonsterEggVariantCobblestone
  | MonsterEggVariantStoneBrick
  | MonsterEggVariantMossyBrick
  | MonsterEggVariantCrackedBrick
  | MonsterEggVariantChiseledBrick
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData MonsterEggVariant

data MushroomVariant
  = MushroomVariantE
  | MushroomVariantN
  | MushroomVariantNE
  | MushroomVariantNW
  | MushroomVariantS
  | MushroomVariantSE
  | MushroomVariantSW
  | MushroomVariantW
  | MushroomVariantCenter
  | MushroomVariantStem
  | MushroomVariantAllInside
  | MushroomVariantAllOutside
  | MushroomVariantAllStem
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData MushroomVariant

data NetherWartAge
  = NetherWartAge0
  | NetherWartAge1
  | NetherWartAge2
  | NetherWartAge3
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData NetherWartAge

data PistonHeadType
  = PistonHeadNormal
  | PistonHeadSticky
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData PistonHeadType

data NetherPortalAxis
  = NetherPortalAxisX
  | NetherPortalAxisZ
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData NetherPortalAxis

data PotatoAge
  = PotatoAge0
  | PotatoAge1
  | PotatoAge2
  | PotatoAge3
  | PotatoAge4
  | PotatoAge5
  | PotatoAge6
  | PotatoAge7
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData PotatoAge

data PrismarineVariant
  = PrismarinePlain
  | PrismarineBricks
  | PrismarineDark
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData PrismarineVariant

data PumpkinStemAge
  = PumpkinStemAge0
  | PumpkinStemAge1
  | PumpkinStemAge2
  | PumpkinStemAge3
  | PumpkinStemAge4
  | PumpkinStemAge5
  | PumpkinStemAge6
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData PumpkinStemAge

data QuartzVariant
  = QuartzDefault
  | QuartzChiseled
  | QuartzLinesX
  | QuartzLinesY
  | QuartzLinesZ
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData QuartzVariant

data RailShape
  = RailNS
  | RailEW
  | RailNE
  | RailNW
  | RailSE
  | RailSW
  | RailAscendingN
  | RailAscendingS
  | RailAscendingE
  | RailAscendingW
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData RailShape

data UtilityRailShape
  = UtilityRailNS
  | UtilityRailEW
  | UtilityRailAscendingN
  | UtilityRailAscendingS
  | UtilityRailAscendingE
  | UtilityRailAscendingW
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData UtilityRailShape

data RedSandstoneType
  = RedSandstonePlain
  | RedSandstoneSmooth
  | RedSandstoneChiseled
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData RedSandstoneType

data ComparatorMode = Compare | Subtract
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData ComparatorMode

-- Still need redstone facings

data RedstonePower
  = RedstonePower0
  | RedstonePower1
  | RedstonePower2
  | RedstonePower3
  | RedstonePower4
  | RedstonePower5
  | RedstonePower6
  | RedstonePower7
  | RedstonePower8
  | RedstonePower9
  | RedstonePower10
  | RedstonePower11
  | RedstonePower12
  | RedstonePower13
  | RedstonePower14
  | RedstonePower15
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData RedstonePower

data RedstoneRepeaterDelay
  = RedstoneRepeaterDelay1
  | RedstoneRepeaterDelay2
  | RedstoneRepeaterDelay3
  | RedstoneRepeaterDelay4
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData RedstoneRepeaterDelay

data SandVariant
  = SandPlain
  | SandRed
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData SandVariant

data SandstoneType
  = SandstonePlain
  | SandstoneChiseled
  | SandstoneSmooth
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData SandstoneType

type SaplingStage = Bool

data SaplingType
  = SaplingOak
  | SaplingSpruce
  | SaplingBirch
  | SaplingJungle
  | SaplingAcacia
  | SaplingDarkOak
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData SaplingType

data SignStandingRotation
  = SignStandingS
  | SignStandingSSW
  | SignStandingSW
  | SignStandingWSW
  | SignStandingW
  | SignStandingWNW
  | SignStandingNW
  | SignStandingNNW
  | SignStandingN
  | SignStandingNNE
  | SignStandingNE
  | SignStandingENE
  | SignStandingE
  | SignStandingESE
  | SignStandingSE
  | SignStandingSSE
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData SignStandingRotation

data StoneSlabVariant
  = StoneSlabPlain
  | StoneSlabSandstone
  | StoneSlabWoodOld
  | StoneSlabCobblestone
  | StoneSlabBricks
  | StoneSlabStoneBrick
  | StoneSlabNetherBrick
  | StoneSlabQuartz
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData StoneSlabVariant

data StoneSlab2Variant = StoneSlabRedSandstone
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData StoneSlab2Variant

data WoodenSlabVariant
  = WoodSlabOak
  | WoodSlabSpruce
  | WoodSlabBirch
  | WoodSlabJungle
  | WoodSlabDarkOak
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData WoodenSlabVariant

data PurpurSlabVariant = PurpurSlabDefault
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData PurpurSlabVariant

type Seamless = Bool

data DoubleStoneSlabVariant
  = DoubleStoneSlabPlain
  | DoubleSandstoneSlab
  | DoubleWoodOldSlab
  | DoubleCobblestoneSlab
  | DoubleBricksSlab
  | DoubleStoneBrickSlab
  | DoubleNetherBrickSlab
  | DoubleQuartzSlab
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData DoubleStoneSlabVariant

data DoubleStoneSlab2Variant = DoubleStoneSlab2RedSandstone
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData DoubleStoneSlab2Variant

data DoubleWoodenSlabVariant
  = DoubleWoodenSlabOak
  | DoubleWoodenSlabSpruce
  | DoubleWoodenSlabBirch
  | DoubleWoodenSlabJungle
  | DoubleWoodenSlabAcacia
  | DoubleWoodenSlabDarkOak
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData DoubleWoodenSlabVariant

data PurpurDoubleSlabVariant = PurpurDoubleSlabDefault
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData PurpurDoubleSlabVariant

data SnowLayers
  = SnowLayer1
  | SnowLayer2
  | SnowLayer3
  | SnowLayer4
  | SnowLayer5
  | SnowLayer6
  | SnowLayer7
  | SnowLayer8
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData SnowLayers

type SpongeWet = Bool

data StairShape
  = StairStraight
  | StairInnerLeft
  | StairInnerRight
  | StairOuterLeft
  | StairOuterRight
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData StairShape

data StoneVariant
  = StonePlain
  | Granite
  | PolishedGranite
  | Diorite
  | PolishedDiorite
  | Andesite
  | PolishedAndesite
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData StoneVariant

data StoneBrickVariant
  = StoneBrickPlain
  | StoneBrickMossy
  | StoneBrickCracked
  | StoneBrickChiseled
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData StoneBrickVariant

data StructureBlockMode
  = StructureBlockSave
  | StructureBlockLoad
  | StructureBlockCorner
  | StructureBlockData
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData StructureBlockMode

data SugarCaneAge
  = SugarCaneAge0
  | SugarCaneAge1
  | SugarCaneAge2
  | SugarCaneAge3
  | SugarCaneAge4
  | SugarCaneAge5
  | SugarCaneAge6
  | SugarCaneAge7
  | SugarCaneAge8
  | SugarCaneAge9
  | SugarCaneAge10
  | SugarCaneAge11
  | SugarCaneAge12
  | SugarCaneAge13
  | SugarCaneAge14
  | SugarCaneAge15
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData SugarCaneAge

data DoublePlantVariant
  = DoublePlantSunflower
  | DoublePlantSyringa
  | DoublePlantDoubleGrass
  | DoublePlantDoubleFern
  | DoublePlantDoubleRose
  | DoublePlantPaeonia
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData DoublePlantVariant

data WeightedPressurePlatePower
  = WeightedPressurePlatePower0
  | WeightedPressurePlatePower1
  | WeightedPressurePlatePower2
  | WeightedPressurePlatePower3
  | WeightedPressurePlatePower4
  | WeightedPressurePlatePower5
  | WeightedPressurePlatePower6
  | WeightedPressurePlatePower7
  | WeightedPressurePlatePower8
  | WeightedPressurePlatePower9
  | WeightedPressurePlatePower10
  | WeightedPressurePlatePower11
  | WeightedPressurePlatePower12
  | WeightedPressurePlatePower13
  | WeightedPressurePlatePower14
  | WeightedPressurePlatePower15
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData WeightedPressurePlatePower

data WheatAge
  = WheatAge0
  | WheatAge1
  | WheatAge2
  | WheatAge3
  | WheatAge4
  | WheatAge5
  | WheatAge6
  | WheatAge7
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData WheatAge

data LogAxis
  = LogAxisNone
  | LogAxisX
  | LogAxisY
  | LogAxisZ
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData LogAxis

data LogVariant
  = WoodOak
  | WoodSpruce
  | WoodBirch
  | WoodJungle
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData LogVariant

data Log2Variant
  = WoodAcacia
  | WoodDarkOak
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData Log2Variant

data WoodPlanksVariant
  = WoodPlanksOak
  | WoodPlanksSpruce
  | WoodPlanksBirch
  | WoodPlanksJungle
  | WoodPlanksAcacia
  | WoodPlanksDarkOak
  deriving (Show,Eq,Ord,Enum,Generic,Data,Typeable)

instance NFData WoodPlanksVariant

data RedSandstoneSlabs
  = SlabRedSandstone
  | SlabUpperRedSandstone
  deriving (Show,Eq)
