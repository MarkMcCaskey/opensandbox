-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data.Biome
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data.Biome
  ( Biome (..)
  , BiomeID (..)
  , ocean
  , plains
  , desert
  , extremeHills
  , forest
  , taiga
  , swampland
  , river
  , hell
  , theEnd
  , frozenOcean
  , frozenRiver
  , icePlains
  , iceMountains
  , mushroomIsland
  , mushroomIslandShore
  , beach
  , desertHills
  , forestHills
  , taigaHills
  , extremeHillsEdge
  , jungle
  , jungleHills
  , jungleEdge
  , deepOcean
  , stoneBeach
  , coldBeach
  , birchForest
  , birchForestHills
  , roofedForest
  , coldTaiga
  , coldTaigaHills
  , megaTaiga
  , megaTaigaHills
  , extremeHillsPlus
  , savanna
  , savannaPlateau
  , mesa
  , mesaPlateauF
  , redwoodTaigaHillsM
  ) where

import Data.Word

data Biome = Biome
  { biomeID           :: BiomeID
  , biomeColor        :: Word32
  , biomeRainfall     :: Double
  , biomeTemperature  :: Double
  } deriving (Show,Eq)

data BiomeID
  = BiomeIDOcean
  | BiomeIDPlains
  | BiomeIDDesert
  | BiomeIDExtremeHills
  | BiomeIDForest
  | BiomeIDTaiga
  | BiomeIDSwampland
  | BiomeIDRiver
  | BiomeIDHell
  | BiomeIDTheEnd
  | BiomeIDFrozenOcean
  | BiomeIDFrozenRiver
  | BiomeIDIcePlains
  | BiomeIDIceMountains
  | BiomeIDMushroomIsland
  | BiomeIDMushroomIslandShore
  | BiomeIDBeach
  | BiomeIDDesertHills
  | BiomeIDForestHills
  | BiomeIDTaigaHills
  | BiomeIDExtremeHillsEdge
  | BiomeIDJungle
  | BiomeIDJungleHills
  | BiomeIDJungleEdge
  | BiomeIDDeepOcean
  | BiomeIDStoneBeach
  | BiomeIDColdBeach
  | BiomeIDBirchForest
  | BiomeIDBirchForestHills
  | BiomeIDRoofedForest
  | BiomeIDColdTaiga
  | BiomeIDColdTaigaHills
  | BiomeIDMegaTaiga
  | BiomeIDMegaTaigaHills
  | BiomeIDExtremeHillsPlus
  | BiomeIDSavanna
  | BiomeIDSavannaPlateau
  | BiomeIDMesa
  | BiomeIDMesaPlateauF
  | BiomeIDMesaPlateau
  | BiomeIDTheVoid
  | BiomeIDPlainsM
  | BiomeIDSunflowerPlains
  | BiomeIDDesertM
  | BiomeIDExtremeHillsM
  | BiomeIDFlowerForest
  | BiomeIDTaigaM
  | BiomeIDSwamplandM
  | BiomeIDIcePlainsSpikes
  | BiomeIDJungleM
  | BiomeIDJungleEdgeM
  | BiomeIDBirchForestM
  | BiomeIDBirchForestHillsM
  | BiomeIDRoofedForestM
  | BiomeIDColdTaigaM
  | BiomeIDMegaSpruceTaiga
  | BiomeIDRedwoodTaigaHillsM
  | BiomeIDExtremeHillsPlusM
  | BiomeIDSavannaM
  | BiomeIDSavannaPlateauM
  | BiomeIDMesaBryce
  | BiomeIDMesaPlateauFM
  | BiomeIDMesaPlateauM
  deriving (Show,Eq)

instance Enum BiomeID where
  fromEnum BiomeIDOcean = 0
  fromEnum BiomeIDPlains = 1
  fromEnum BiomeIDDesert = 2
  fromEnum BiomeIDExtremeHills = 3
  fromEnum BiomeIDForest = 4
  fromEnum BiomeIDTaiga = 5
  fromEnum BiomeIDSwampland = 6
  fromEnum BiomeIDRiver = 7
  fromEnum BiomeIDHell = 8
  fromEnum BiomeIDTheEnd = 9
  fromEnum BiomeIDFrozenOcean = 10
  fromEnum BiomeIDFrozenRiver = 11
  fromEnum BiomeIDIcePlains = 12
  fromEnum BiomeIDIceMountains = 13
  fromEnum BiomeIDMushroomIsland = 14
  fromEnum BiomeIDMushroomIslandShore = 15
  fromEnum BiomeIDBeach = 16
  fromEnum BiomeIDDesertHills = 17
  fromEnum BiomeIDForestHills = 18
  fromEnum BiomeIDTaigaHills = 19
  fromEnum BiomeIDExtremeHillsEdge = 20
  fromEnum BiomeIDJungle = 21
  fromEnum BiomeIDJungleHills = 22
  fromEnum BiomeIDJungleEdge = 23
  fromEnum BiomeIDDeepOcean = 24
  fromEnum BiomeIDStoneBeach = 25
  fromEnum BiomeIDColdBeach = 26
  fromEnum BiomeIDBirchForest = 27
  fromEnum BiomeIDBirchForestHills = 28
  fromEnum BiomeIDRoofedForest = 29
  fromEnum BiomeIDColdTaiga = 30
  fromEnum BiomeIDColdTaigaHills = 31
  fromEnum BiomeIDMegaTaiga = 32
  fromEnum BiomeIDMegaTaigaHills = 33
  fromEnum BiomeIDExtremeHillsPlus = 34
  fromEnum BiomeIDSavanna = 35
  fromEnum BiomeIDSavannaPlateau = 36
  fromEnum BiomeIDMesa = 37
  fromEnum BiomeIDMesaPlateauF = 38
  fromEnum BiomeIDMesaPlateau = 39
  fromEnum BiomeIDTheVoid = 127
  fromEnum BiomeIDPlainsM = 128
  fromEnum BiomeIDSunflowerPlains = 129
  fromEnum BiomeIDDesertM = 130
  fromEnum BiomeIDExtremeHillsM = 131
  fromEnum BiomeIDFlowerForest = 132
  fromEnum BiomeIDTaigaM = 133
  fromEnum BiomeIDSwamplandM = 134
  fromEnum BiomeIDIcePlainsSpikes = 140
  fromEnum BiomeIDJungleM = 149
  fromEnum BiomeIDJungleEdgeM = 151
  fromEnum BiomeIDBirchForestM = 155
  fromEnum BiomeIDBirchForestHillsM = 156
  fromEnum BiomeIDRoofedForestM = 157
  fromEnum BiomeIDColdTaigaM = 158
  fromEnum BiomeIDMegaSpruceTaiga = 160
  fromEnum BiomeIDRedwoodTaigaHillsM = 161
  fromEnum BiomeIDExtremeHillsPlusM = 162
  fromEnum BiomeIDSavannaM = 163
  fromEnum BiomeIDSavannaPlateauM = 164
  fromEnum BiomeIDMesaBryce = 165
  fromEnum BiomeIDMesaPlateauFM = 166
  fromEnum BiomeIDMesaPlateauM = 167

  toEnum 0 = BiomeIDOcean
  toEnum 1 = BiomeIDPlains
  toEnum 2 = BiomeIDDesert
  toEnum 3 = BiomeIDExtremeHills
  toEnum 4 = BiomeIDForest
  toEnum 5 = BiomeIDTaiga
  toEnum 6 = BiomeIDSwampland
  toEnum 7 = BiomeIDRiver
  toEnum 8 = BiomeIDHell
  toEnum 9 = BiomeIDTheEnd
  toEnum 10 = BiomeIDFrozenOcean
  toEnum 11 = BiomeIDFrozenRiver
  toEnum 12 = BiomeIDIcePlains
  toEnum 13 = BiomeIDIceMountains
  toEnum 14 = BiomeIDMushroomIsland
  toEnum 15 = BiomeIDMushroomIslandShore
  toEnum 16 = BiomeIDBeach
  toEnum 17 = BiomeIDDesertHills
  toEnum 18 = BiomeIDForestHills
  toEnum 19 = BiomeIDTaigaHills
  toEnum 20 = BiomeIDExtremeHillsEdge
  toEnum 21 = BiomeIDJungle
  toEnum 22 = BiomeIDJungleHills
  toEnum 23 = BiomeIDJungleEdge
  toEnum 24 = BiomeIDDeepOcean
  toEnum 25 = BiomeIDStoneBeach
  toEnum 26 = BiomeIDColdBeach
  toEnum 27 = BiomeIDBirchForest
  toEnum 28 = BiomeIDBirchForestHills
  toEnum 29 = BiomeIDRoofedForest
  toEnum 30 = BiomeIDColdTaiga
  toEnum 31 = BiomeIDColdTaigaHills
  toEnum 32 = BiomeIDMegaTaiga
  toEnum 33 = BiomeIDMegaTaigaHills
  toEnum 34 = BiomeIDExtremeHillsPlus
  toEnum 35 = BiomeIDSavanna
  toEnum 36 = BiomeIDSavannaPlateau
  toEnum 37 = BiomeIDMesa
  toEnum 38 = BiomeIDMesaPlateauF
  toEnum 39 = BiomeIDMesaPlateau
  toEnum 127 = BiomeIDTheVoid
  toEnum 128 = BiomeIDPlainsM
  toEnum 129 = BiomeIDSunflowerPlains
  toEnum 130 = BiomeIDDesertM
  toEnum 131 = BiomeIDExtremeHillsM
  toEnum 132 = BiomeIDFlowerForest
  toEnum 133 = BiomeIDTaigaM
  toEnum 134 = BiomeIDSwamplandM
  toEnum 140 = BiomeIDIcePlainsSpikes
  toEnum 149 = BiomeIDJungleM
  toEnum 151 = BiomeIDJungleEdgeM
  toEnum 155 = BiomeIDBirchForestM
  toEnum 156 = BiomeIDBirchForestHillsM
  toEnum 157 = BiomeIDRoofedForestM
  toEnum 158 = BiomeIDColdTaigaM
  toEnum 160 = BiomeIDMegaSpruceTaiga
  toEnum 161 = BiomeIDRedwoodTaigaHillsM
  toEnum 162 = BiomeIDExtremeHillsPlusM
  toEnum 163 = BiomeIDSavannaM
  toEnum 164 = BiomeIDSavannaPlateauM
  toEnum 165 = BiomeIDMesaBryce
  toEnum 166 = BiomeIDMesaPlateauFM
  toEnum 167 = BiomeIDMesaPlateauM

ocean :: Biome
ocean = Biome BiomeIDOcean 112 0.5 0.5

plains :: Biome
plains = Biome BiomeIDPlains 9286496 0.4 0.8

desert :: Biome
desert = Biome BiomeIDDesert 16421912 0 2

extremeHills :: Biome
extremeHills = Biome BiomeIDExtremeHills 6316128 0.3 0.2

forest :: Biome
forest = Biome BiomeIDForest 353825 0.8 0.7

taiga :: Biome
taiga = Biome BiomeIDTaiga 747097 0.8 0.05

swampland :: Biome
swampland = Biome BiomeIDSwampland 522674 0.9 0.8

river :: Biome
river = Biome BiomeIDRiver 255 0.5 0.5

hell :: Biome
hell = Biome BiomeIDHell 16711680 0 2

theEnd :: Biome
theEnd = Biome BiomeIDTheEnd 8421631 0.5 0.5

frozenOcean :: Biome
frozenOcean = Biome BiomeIDFrozenOcean 9474208 0.5 0

frozenRiver :: Biome
frozenRiver = Biome BiomeIDFrozenRiver 10526975 0.5 0

icePlains :: Biome
icePlains = Biome BiomeIDIcePlains 16777215 0.5 0

iceMountains :: Biome
iceMountains = Biome BiomeIDIceMountains 10526880 0.5 0

mushroomIsland :: Biome
mushroomIsland = Biome BiomeIDMushroomIsland 16711935 1 0.9

mushroomIslandShore :: Biome
mushroomIslandShore = Biome BiomeIDMushroomIslandShore 10486015 1 0.9

beach :: Biome
beach = Biome BiomeIDBeach 16440917 0.4 0.8

desertHills :: Biome
desertHills = Biome BiomeIDDesertHills 13786898 0 2

forestHills :: Biome
forestHills = Biome BiomeIDForestHills 2250012 0.8 0.7

taigaHills :: Biome
taigaHills = Biome BiomeIDTaigaHills 1456435 0.7 0.2

extremeHillsEdge :: Biome
extremeHillsEdge = Biome BiomeIDExtremeHillsEdge 7501978 0.3 0.2

jungle :: Biome
jungle = Biome BiomeIDJungle 5470985 0.9 1.2

jungleHills :: Biome
jungleHills = Biome BiomeIDJungleHills 2900485 0.9 1.2

jungleEdge :: Biome
jungleEdge = Biome BiomeIDJungleEdge 6458135 0.8 0.95

deepOcean :: Biome
deepOcean = Biome BiomeIDDeepOcean 48 0.5 0.5

stoneBeach :: Biome
stoneBeach = Biome BiomeIDStoneBeach 10658436 0.3 0.2

coldBeach :: Biome
coldBeach = Biome BiomeIDColdBeach 16445632 0.3 0.05

birchForest :: Biome
birchForest = Biome BiomeIDBirchForest 3175492 0.6 0.6

birchForestHills :: Biome
birchForestHills = Biome BiomeIDBirchForestHills 2055986 0.6 0.6

roofedForest :: Biome
roofedForest = Biome BiomeIDRoofedForest 4215066 0.8 0.7

coldTaiga :: Biome
coldTaiga = Biome BiomeIDColdTaiga 3233098 0.5 (-0.5)

coldTaigaHills :: Biome
coldTaigaHills = Biome BiomeIDColdTaigaHills 2375478 0.4 (-0.5)

megaTaiga :: Biome
megaTaiga = Biome BiomeIDMegaTaiga 5858897 0.8 0.3

megaTaigaHills :: Biome
megaTaigaHills = Biome BiomeIDMegaTaigaHills 4542270 0.8 0.3

extremeHillsPlus :: Biome
extremeHillsPlus = Biome BiomeIDExtremeHillsPlus 5271632 0.3 0.2

savanna :: Biome
savanna = Biome BiomeIDSavanna 12431967 0 1.2

savannaPlateau :: Biome
savannaPlateau = Biome BiomeIDSavannaPlateau 10984804 0 1

mesa :: Biome
mesa = Biome BiomeIDMesa 14238997 0.5 2.0

mesaPlateauF :: Biome
mesaPlateauF = Biome BiomeIDMesaPlateauF 11573093 0.5 0.2

redwoodTaigaHillsM :: Biome
redwoodTaigaHillsM = Biome BiomeIDRedwoodTaigaHillsM 13274213 0.5 2.0
