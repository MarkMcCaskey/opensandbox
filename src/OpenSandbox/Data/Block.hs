{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module OpenSandbox.Data.Block
  ( BlockID (..)
  ) where

import            Control.DeepSeq
import            GHC.Generics (Generic)

data BlockID
  = BlockIDAir
  | BlockIDStone StoneVariant
  | BlockIDGrass Snowy GrassType
  | BlockIDDirt Snowy DirtVariant
  | BlockIDCobblestone
  | BlockIDWoodPlanks WoodPlanksVariant
  | BlockIDSapling SaplingStage SaplingType
  | BlockIDBedrock
  | BlockIDFlowingWater FluidLevel
  | BlockIDWater FluidLevel
  | BlockIDFlowingLava FluidLevel
  | BlockIDLava FluidLevel
  | BlockIDSand SandVariant
  | BlockIDGravel
  | BlockIDGoldOre
  | BlockIDIronOre
  | BlockIDCoalOre
  | BlockIDLog LogAxis LogVariant
  | BlockIDLeaves Bool Bool LeavesVariant
  | BlockIDSponge SpongeWet
  | BlockIDGlass
  | BlockIDLapisOre
  | BlockIDLapisBlock
  | BlockIDDispenser DispenserFacing Bool
  | BlockIDSandstone SandstoneType
  | BlockIDNoteblock
  | BlockIDBed Facing Occupied BedPart
  | BlockIDGoldenRail Bool UtilityRailShape
  | BlockIDDetectorRail Bool UtilityRailShape
  | BlockIDStickyPiston Bool PistonFacing
  | BlockIDWeb
  | BlockIDTallgrass GrassType
  | BlockIDDeadbush
  | BlockIDPiston Bool PistonFacing
  | BlockIDPistonHead PistonHeadFacing Bool PistonHeadType
  | BlockIDWool Color
  | BlockIDPistonExtension Bool PistonExtensionFacing
  | BlockIDYellowFlower
  | BlockIDRedFlower RedFlowersType
  | BlockIDBrownMushroom
  | BlockIDRedMushroom
  | BlockIDGoldBlock
  | BlockIDIronBlock
  | BlockIDDoubleStoneSlab Bool DoubleStoneSlabVariant
  | BlockIDStoneSlab Bool StoneSlabVariant
  | BlockIDBrickBlock
  | BlockIDTNT Bool
  | BlockIDBookshelf
  | BlockIDMossyCobblestone
  | BlockIDObsidian
  | BlockIDTorch TorchFacing
  | BlockIDFire FireAge Bool Bool Bool Bool Bool
  | BlockIDMobSpawner
  | BlockIDOakStairs Facing Half StairShape
  | BlockIDChest Facing
  | BlockIDRedstoneWire RedstonePower
  | BlockIDDiamondOre
  | BlockIDDiamondBlock
  | BlockIDCraftingTable
  | BlockIDWheat WheatAge
  | BlockIDFarmland FarmlandMoisture
  | BlockIDFurnace Facing
  | BlockIDLitFurnace Facing
  | BlockIDStandingSign SignStandingRotation
  | BlockIDWoodenDoor Facing DoorHalf DoorHinge Bool Bool
  | BlockIDLadder Facing
  | BlockIDRail RailShape
  | BlockIDStoneStairs Facing Half StairShape
  | BlockIDWallSign Facing
  | BlockIDLever LeverFacing Bool
  | BlockIDStonePressurePlate Bool
  | BlockIDIronDoor Facing DoorHalf DoorHinge Bool Bool
  | BlockIDWoodenPressurePlate Bool
  | BlockIDRedstoneOre
  | BlockIDLitRedstoneOre TorchFacing
  | BlockIDUnlitRedstoneTorch TorchFacing
  | BlockIDRedstoneTorch TorchFacing
  | BlockIDStoneButton ButtonFacing Bool
  | BlockIDSnowLayer SnowLayers
  | BlockIDIce
  | BlockIDSnow
  | BlockIDCactus CactusAge
  | BlockIDClay
  | BlockIDReeds SugarCaneAge
  | BlockIDJukebox Bool
  | BlockIDFence
  | BlockIDPumpkin Facing
  | BlockIDNetherrack
  | BlockIDSoulSand
  | BlockIDGlowstone
  | BlockIDPortal
  | BlockIDLitPumpkin Facing
  | BlockIDCake CakeBites
  | BlockIDUnpoweredRepeater RedstoneRepeaterDelay Facing Bool
  | BlockIDPoweredRepeater RedstoneRepeaterDelay Facing Bool
  | BlockIDStainedGlass Color
  | BlockIDTrapdoor Facing Half Bool
  | BlockIDMonsterEgg MonsterEggVariant
  | BlockIDStoneBrick StoneBrickVariant
  | BlockIDBrownMushroomBlock MushroomVariant
  | BlockIDRedMushroomBlock MushroomVariant
  | BlockIDIronBars
  | BlockIDGlassPane
  | BlockIDMelonBlock
  | BlockIDPumpkinStem PumpkinStemAge PumpkinStemFacing
  | BlockIDMelonStem MelonStemAge MelonStemFacing
  | BlockIDVine Bool Bool Bool Bool Bool
  | BlockIDFenceGate Facing Bool Bool Bool
  | BlockIDBrickStairs Facing Half StairShape
  | BlockIDStoneBrickStairs Facing Half StairShape
  | BlockIDMycelium
  | BlockIDWaterlily
  | BlockIDNetherBrick
  | BlockIDNetherBrickFence
  | BlockIDNetherBrickStairs Facing Half StairShape
  | BlockIDNetherWart NetherWartAge
  | BlockIDEnchantingTable
  | BlockIDBrewingStand Bool Bool Bool
  | BlockIDCauldron CauldronLevel
  | BlockIDEndPortal
  | BlockIDEndPortalFrame Bool Facing
  | BlockIDEndStone
  | BlockIDDragonEgg
  | BlockIDRedstoneLamp
  | BlockIDLitRedstoneLamp
  | BlockIDDoubleWoodenSlab DoubleWoodenSlabVariant
  | BlockIDWoodenSlab Half WoodenSlabVariant
  | BlockIDCocoa CocoaAge Facing
  | BlockIDSandstoneStairs Facing Half StairShape
  | BlockIDEmeraldOre
  | BlockIDEnderChest Facing
  | BlockIDTripwireHook Bool Facing Bool
  | BlockIDTripwire Bool Bool Bool Bool Bool Bool Bool
  | BlockIDEmeraldBlock
  | BlockIDSpruceStairs Facing Half StairShape
  | BlockIDBirchStairs Facing Half StairShape
  | BlockIDJungleStairs Facing Half StairShape
  | BlockIDCommandBlock Bool CommandBlockFacing
  | BlockIDBeacon
  | BlockIDCobblestoneWall Bool Bool Bool Bool Bool CobblestoneWallVariant
  | BlockIDFlowerPot FlowerPotContents
  | BlockIDCarrots CarrotAge
  | BlockIDPotatoes PotatoAge
  | BlockIDWoodenButton ButtonFacing Bool
  | BlockIDSkull SkullFacing Bool
  | BlockIDAnvil AnvilDamage Facing
  | BlockIDTrappedChest Facing
  | BlockIDLightWeightedPressurePlate WeightedPressurePlatePower
  | BlockIDHeavyWeightedPressurePlate WeightedPressurePlatePower
  | BlockIDUnpoweredComparator Facing ComparatorMode Bool
  | BlockIDPoweredComparator Facing ComparatorMode Bool
  | BlockIDDaylightDetector DaylightSensorPower
  | BlockIDRedstoneBlock
  | BlockIDQuartzOre
  | BlockIDHopper Bool HopperFacing
  | BlockIDQuartzBlock QuartzVariant
  | BlockIDQuartzStairs Facing Half StairShape
  | BlockIDActivatorRail Bool UtilityRailShape
  | BlockIDDropper DropperFacing Bool
  | BlockIDStainedHardenedClay Color
  | BlockIDStainedGlassPane Color Bool Bool Bool Bool
  | BlockIDLeaves2 Bool Bool Leaves2Variant
  | BlockIDLog2 LogAxis Log2Variant
  | BlockIDAcaciaStairs Facing Half StairShape
  | BlockIDDarkOakStairs Facing Half StairShape
  | BlockIDSlime
  | BlockIDBarrier
  | BlockIDIronTrapdoor Facing Half Bool
  | BlockIDPrismarine PrismarineVariant
  | BlockIDSeaLantern
  | BlockIDHayBlock Axis
  | BlockIDCarpet Color
  | BlockIDHardenedClay
  | BlockIDCoalBlock
  | BlockIDPackedIce
  | BlockIDDoublePlant Half DoublePlantVariant Facing
  | BlockIDStandingBanner BannerStanding
  | BlockIDWallBanner Facing
  | BlockIDDaylightDetectorInverted DaylightSensorPower
  | BlockIDRedSandstone RedSandstoneType
  | BlockIDRedSandstoneStairs Facing Half StairShape
  | BlockIDDoubleStoneSlab2 Half DoubleStoneSlab2Variant
  | BlockIDStoneSlab2 Half StoneSlab2Variant
  | BlockIDSpruceFenceGate Facing Bool Bool Bool
  | BlockIDBirchFenceGate Facing Bool Bool Bool
  | BlockIDJungleFenceGate Facing Bool Bool Bool
  | BlockIDDarkOakFenceGate Facing Bool Bool Bool
  | BlockIDAcaciaFenceGate Facing Bool Bool Bool
  | BlockIDSpruceFence Bool Bool Bool Bool
  | BlockIDBirchFence Bool Bool Bool Bool
  | BlockIDJungleFence Bool Bool Bool Bool
  | BlockIDDarkOakFence Bool Bool Bool Bool
  | BlockIDAcaciaFence Bool Bool Bool Bool
  | BlockIDSpruceDoor Facing DoorHalf DoorHinge Bool Bool
  | BlockIDBirchDoor Facing DoorHalf DoorHinge Bool Bool
  | BlockIDJungleDoor Facing DoorHalf DoorHinge Bool Bool
  | BlockIDAcaciaDoor Facing DoorHalf DoorHinge Bool Bool
  | BlockIDDarkOakDoor Facing DoorHalf DoorHinge Bool Bool
  | BlockIDEndRod EndRodFacing
  | BlockIDChorusPlant Bool Bool Bool Bool Bool Bool
  | BlockIDChorusFlower ChorusFlowerAge
  | BlockIDPurpurBlock Axis
  | BlockIDPurpurPillar Axis
  | BlockIDPurpurStairs Facing Half StairShape
  | BlockIDPurpurDoubleSlab PurpurDoubleSlabVariant
  | BlockIDPurpurSlab PurpurSlabVariant
  | BlockIDEndBricks
  | BlockIDBeetroots BeetrootAge
  | BlockIDGrassPath
  | BlockIDEndGateway
  | BlockIDRepeatingCommandBlock Bool CommandBlockFacing
  | BlockIDChainCommandBlock Bool CommandBlockFacing
  | BlockIDFrostedIce FrostedIceAge
  | BlockIDMagma
  | BlockIDNetherWartBlock
  | BlockIDRedNetherBrick
  | BlockIDBoneBlock Axis
  | BlockIDStructureVoid StructureBlockMode
  | BlockIDStructureBlock StructureBlockMode
  deriving (Show,Eq)

data Axis
  = AxisX
  | AxisY
  | AxisZ
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData Axis

data AnvilDamage
  = Anvil
  | SlightlyDamagedAnvil
  | VeryDamagedAnvil
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData BannerStanding

newtype Occupied = Occupied Bool
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData Occupied

data BedPart = Head | Foot
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData BedPart

data BeetrootAge
  = BeetrootAgeI
  | BeetrootAgeII
  | BeetrootAgeIII
  | BeetrootAgeIV
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData BeetrootAge

newtype HasBottle = HasBottle Bool deriving (Show,Eq,Ord,Enum,Generic)

instance NFData HasBottle

data ButtonFacing
  = ButtonFacingN
  | ButtonFacingS
  | ButtonFacingE
  | ButtonFacingW
  | ButtonFacingUp
  | ButtonFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData ButtonFacing

newtype ButtonPowered = ButtonPowered Bool deriving (Show,Eq,Ord,Enum)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData CactusAge

data CakeBites
  = CakeBites0
  | CakeBites1
  | CakeBites2
  | CakeBites3
  | CakeBites4
  | CakeBites5
  | CakeBites6
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData CarrotAge

data CauldronLevel
  = CauldronLevel0
  | CauldronLevel1
  | CauldronLevel2
  | CauldronLevel3
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData CauldronLevel

data ChorusFlowerAge
  = ChorusFlowerAge0
  | ChorusFlowerAge1
  | ChorusFlowerAge2
  | ChorusFlowerAge3
  | ChorusFlowerAge4
  | ChorusFlowerAge5
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData ChorusFlowerAge

data CobblestoneWallVariant
  = CobblestoneWall
  | MossyCobblestoneWall
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData CobblestoneWallVariant

data CocoaAge
  = CocoaAge0
  | CocoaAge1
  | CocoaAge2
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData CocoaAge

data CommandBlockFacing
  = CommandBlockFacingN
  | CommandBlockFacingS
  | CommandBlockFacingE
  | CommandBlockFacingW
  | CommandBlockFacingUp
  | CommandBlockFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData CommandBlockFacing

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DaylightSensorPower

data DirtVariant
  = Dirt
  | Coarse
  | Podzol
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DirtVariant

data DispenserFacing
  = DispenserFacingN
  | DispenserFacingS
  | DispenserFacingE
  | DispenserFacingW
  | DispenserFacingUp
  | DispenserFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DispenserFacing

data DoorHalf = DoorUpper | DoorLower
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DoorHalf

data DoorHinge = HingeLeft | HingeRight
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DoorHinge

data DropperFacing
  = DropperFacingN
  | DropperFacingS
  | DropperFacingE
  | DropperFacingW
  | DropperFacingUp
  | DropperFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DropperFacing

data EndRodFacing
  = EndRodFacingN
  | EndRodFacingS
  | EndRodFacingE
  | EndRodFacingW
  | EndRodFacingUp
  | EndRodFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData EndRodFacing

data FarmlandMoisture
  = Moisture0
  | Moisture1
  | Moisture2
  | Moisture3
  | Moisture4
  | Moisture5
  | Moisture6
  | Moisture7
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData RedFlowersType

data YellowFlowersType = FlowerDandelion
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData YellowFlowersType

data FlowerPotContents
  = FPC_Empty
  | FPC_Rose
  | FPC_BlueOrchid
  | FPC_Allium
  | FPC_Houstonia
  | FPC_RedTulip
  | FPC_OrangeTulip
  | FPC_WhiteTulip
  | FPC_PinkTulip
  | FPC_OxeyeDaisy
  | FPC_Dandelion
  | FPC_OakSapling
  | FPC_SpruceSapling
  | FPC_BirchSapling
  | FPC_JungleSapling
  | FPC_AcaciaSapling
  | FPC_DarkOakSapling
  | FPC_MushroomRed
  | FPC_MushroomBrown
  | FPC_DeadBush
  | FPC_Fern
  | FPC_Cactus
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData FlowerPotContents

data FrostedIceAge
  = FrostedIceAge0
  | FrostedIceAge1
  | FrostedIceAge2
  | FrostedIceAge3
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData FrostedIceAge

data GrassType
  = GrassDeadBush
  | GrassTallGrass
  | GrassFern
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData GrassType

data HopperFacing
  = HopperFacingN
  | HopperFacingS
  | HopperFacingE
  | HopperFacingW
  | HopperFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData FluidLevel

data LeavesVariant
  = LeavesOak
  | LeavesSpruce
  | LeavesBirch
  | LeavesJungle
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData LeavesVariant

data Leaves2Variant
  = Leaves2Acacia
  | Leaves2DarkOak
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData MelonStemAge

data MelonStemFacing
  = MelonStemFacingN
  | MelonStemFacingS
  | MelonStemFacingE
  | MelonStemFacingW
  | MelonStemFacingUp
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData MelonStemFacing

data MonsterEggVariant
  = MonsterEggVariantStone
  | MonsterEggVariantCobblestone
  | MonsterEggVariantStoneBrick
  | MonsterEggVariantMossyBrick
  | MonsterEggVariantCrackedBrick
  | MonsterEggVariantChiseledBrick
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData MushroomVariant

data NetherWartAge
  = NetherWartAge0
  | NetherWartAge1
  | NetherWartAge2
  | NetherWartAge3
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData NetherWartAge

data PistonFacing
  = PistonFacingN
  | PistonFacingS
  | PistonFacingE
  | PistonFacingW
  | PistonFacingUp
  | PistonFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData PistonFacing

data PistonHeadFacing
  = PistonHeadFacingN
  | PistonHeadFacingS
  | PistonHeadFacingE
  | PistonHeadFacingW
  | PistonHeadFacingUp
  | PistonHeadFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData PistonHeadFacing

data PistonHeadType
  = PistonHeadNormal
  | PistonHeadSticky
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData PistonHeadType

data PistonExtensionFacing
  = PistonExtensionFacingN
  | PistonExtensionFacingS
  | PistonExtensionFacingE
  | PistonExtensionFacingW
  | PistonExtensionFacingUp
  | PistonExtensionFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData PistonExtensionFacing

data NetherPortalAxis
  = NetherPortalAxisX
  | NetherPortalAxisZ
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData PotatoAge

data PrismarineVariant
  = Prismarine
  | PrismarineBricks
  | DarkPrismarine
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData PrismarineVariant

data PumpkinStemAge
  = PumpkinStemAge0
  | PumpkinStemAge1
  | PumpkinStemAge2
  | PumpkinStemAge3
  | PumpkinStemAge4
  | PumpkinStemAge5
  | PumpkinStemAge6
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData PumpkinStemAge

data PumpkinStemFacing
  = PumpkinStemFacingN
  | PumpkinStemFacingS
  | PumpkinStemFacingE
  | PumpkinStemFacingW
  | PumpkinStemFacingUp
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData PumpkinStemFacing

data QuartzVariant
  = QuartzDefault
  | QuartzChiseled
  | QuartzLinesX
  | QuartzLinesY
  | QuartzLinesZ
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData RailShape

data UtilityRailShape
  = UtilityRailNS
  | UtilityRailEW
  | UtilityRailAscendingN
  | UtilityRailAscendingS
  | UtilityRailAscendingE
  | UtilityRailAscendingW
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData UtilityRailShape

data RedSandstoneType
  = RedSandstone
  | SmoothRedSandstone
  | ChiseledRedSandstone
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData RedSandstoneType

data ComparatorMode = Compare | Subtract
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData RedstonePower

data RedstoneRepeaterDelay
  = RedstoneRepeaterDelay1
  | RedstoneRepeaterDelay2
  | RedstoneRepeaterDelay3
  | RedstoneRepeaterDelay4
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData RedstoneRepeaterDelay

data TorchFacing
  = TorchFacingN
  | TorchFacingS
  | TorchFacingE
  | TorchFacingW
  | TorchFacingUp
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData TorchFacing

data SandVariant
  = Sand
  | RedSand
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData SandVariant

data SandstoneType
  = Sandstone
  | ChiseledSandstone
  | SmoothSandstone
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData SandstoneType

newtype SaplingStage = SaplingStage Bool deriving (Show,Eq,Ord,Enum,Generic)

instance NFData SaplingStage

data SaplingType
  = SaplingOak
  | SaplingSpruce
  | SaplingBirch
  | SaplingJungle
  | SaplingAcacia
  | SaplingDarkOak
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData SignStandingRotation

data SkullFacing
  = SkullFacingN
  | SkullFacingS
  | SkullFacingE
  | SkullFacingW
  | SkullFacingUp
  | SkullFacingDown
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData SkullFacing

data SlabHalf = SlabTop | SlabBottom
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData SlabHalf

data StoneSlabVariant
  = StoneSlab
  | StoneSlabSandstone
  | StoneSlabWoodOld
  | StoneSlabCobblestone
  | StoneSlabBricks
  | StoneSlabStoneBrick
  | StoneSlabNetherBrick
  | StoneSlabQuartz
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData StoneSlabVariant

data StoneSlab2Variant = StoneSlabRedSandstone
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData StoneSlab2Variant

data WoodenSlabVariant
  = WoodSlabOak
  | WoodSlabSpruce
  | WoodSlabBirch
  | WoodSlabJungle
  | WoodSlabDarkOak
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData WoodenSlabVariant

data PurpurSlabVariant = PurpurSlabDefault
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData PurpurSlabVariant

newtype Seamless = Seamless Bool
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData Seamless

data DoubleStoneSlabVariant
  = DoubleStoneSlab
  | DoubleSandstoneSlab
  | DoubleWoodenSlab
  | DoubleCobblestoneSlab
  | DoubleBricksSlab
  | DoubleStoneBrickSlab
  | DoubleNetherBrickSlab
  | DoubleQuartzSlab
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DoubleStoneSlabVariant

data DoubleStoneSlab2Variant = DoubleStoneSlab2RedSandstone
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DoubleStoneSlab2Variant

data DoubleWoodenSlabVariant
  = DoubleWoodenSlabOak
  | DoubleWoodenSlabSpruce
  | DoubleWoodenSlabBirch
  | DoubleWoodenSlabJungle
  | DoubleWoodenSlabAcacia
  | DoubleWoodenSlabDarkOak
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DoubleWoodenSlabVariant

data PurpurDoubleSlabVariant = PurpurDoubleSlabDefault
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData SnowLayers

newtype SpongeWet = SpongeWet Bool
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData SpongeWet

data StairShape
  = StairStraight
  | StairInnerLeft
  | StairInnerRight
  | StairOuterLeft
  | StairOuterRight
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData StairShape

data StoneVariant
  = Stone
  | Granite
  | PolishedGranite
  | Diorite
  | PolishedDiorite
  | Andesite
  | PolishedAndesite
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData StoneVariant

data StoneBrickVariant
  = StoneBrick
  | StoneBrickMossy
  | StoneBrickCracked
  | StoneBrickChiseled
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData StoneBrickVariant

data StructureBlockMode
  = StructureBlockSave
  | StructureBlockLoad
  | StructureBlockCorner
  | StructureBlockData
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData SugarCaneAge

data DoublePlantVariant
  = DoublePlantSunflower
  | DoublePlantSyringa
  | DoublePlantDoubleGrass
  | DoublePlantDoubleFern
  | DoublePlantDoubleRose
  | DoublePlantPaeonia
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData DoublePlantVariant

data Facing
  = FacingN
  | FacingS
  | FacingE
  | FacingW
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData Facing

data Half = Top | Bottom
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData Half

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
  deriving (Show,Eq,Ord,Enum,Generic)

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
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData WheatAge

data LogAxis
  = LogAxisNone
  | LogAxisX
  | LogAxisY
  | LogAxisZ
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData LogAxis

data LogVariant
  = WoodOak
  | WoodSpruce
  | WoodBirch
  | WoodJungle
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData LogVariant

data Log2Variant
  = WoodAcacia
  | WoodDarkOak
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData Log2Variant

data WoodPlanksVariant
  = WoodPlanksOak
  | WoodPlanksSpruce
  | WoodPlanksBirch
  | WoodPlanksJungle
  | WoodPlanksAcacia
  | WoodPlanksDarkOak
  deriving (Show,Eq,Ord,Enum,Generic)

instance NFData WoodPlanksVariant

data RedSandstoneSlabs
  = SlabRedSandstone
  | SlabUpperRedSandstone
  deriving (Show,Eq)

instance Enum RedSandstoneSlabs where
  fromEnum SlabRedSandstone = 0
  fromEnum SlabUpperRedSandstone = 8
  toEnum 0 = SlabRedSandstone
  toEnum 8 = SlabUpperRedSandstone

newtype Snowy = Snowy Bool deriving (Show,Eq,Ord,Enum,Generic)

instance NFData Snowy
