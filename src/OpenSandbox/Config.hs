{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Command
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Config
  ( Config (..)
  , Difficulty (..)
  , GameMode (..)
  , LevelType (..)
  , Compression (..)
  , defaultConfig
  ) where


import qualified  Data.Text as T


data Difficulty = Peaceful | Easy | Normal | Hard
  deriving (Show,Enum,Eq)


data GameMode = Survival | Creative | Adventure | Spectator
  deriving (Show,Enum,Eq)


data LevelType = Default | Flat | LargeBiomes | Amplified | Customized
  deriving (Show,Eq)


data Compression = Everything | Int
  deriving (Show,Eq)


data Config = Config
  { mcAllowFlight                 :: !Bool
  , mcAllowNether                 :: !Bool
  , mcAnnouncePlayerAchievements  :: !Bool
  , mcDifficulty                  :: !Difficulty
  , mcEnableQuery                 :: !Bool
  , mcEnableRcon                  :: !Bool
  , mcEnableCommandBlock          :: !Bool
  , mcForceGamemode               :: !Bool
  , mcGameMode                    :: !GameMode
  , mcGenerateStructures          :: !Bool
  , mcGeneratorSettings           :: !T.Text
  , mcHardcore                    :: !Bool
  , mcLevelName                   :: !T.Text
  , mcLevelSeed                   :: !T.Text
  , mcLevelType                   :: !LevelType
  , mcMaxBuildHeight              :: !Int
  , mcMaxPlayers                  :: !Int
  , mcMaxTickTime                 :: !(Maybe Int)
  , mcMaxWorldSize                :: !Int
  , mcMotd                        :: !T.Text
  , mcNetworkCompressionThreshold :: !(Maybe Compression)
  , mcOnlineMode                  :: !Bool
  , mcOpPermissionLevel           :: !Int
  , mcPlayerIdleTimeout           :: !Int
  , mcPvp                         :: !Bool
  , mcQueryPort                   :: !Int
  , mcRconPassword                :: !T.Text
  , mcResourcePack                :: !T.Text
  , mcResourcePackHash            :: !T.Text
  , mcServerIP                    :: !T.Text
  , mcServerPort                  :: !Int
  , mcSnooperEnabled              :: !Bool
  , mcSpawnAnimals                :: !Bool
  , mcSpawnMonsters               :: !Bool
  , mcSpawnNPCs                   :: !Bool
  , mcSpawnProtection             :: !Int
  , mcUseNativeTransport          :: !Bool
  , mcViewDistance                :: !Int
  , mcWhiteList                   :: !Bool
  } deriving (Show,Eq)


defaultConfig = Config
  { mcAllowFlight                 = False
  , mcAllowNether                 = True
  , mcAnnouncePlayerAchievements  = True
  , mcDifficulty                  = Normal
  , mcEnableQuery                 = False
  , mcEnableRcon                  = False
  , mcEnableCommandBlock          = False
  , mcForceGamemode               = False
  , mcGameMode                    = Survival
  , mcGenerateStructures          = True
  , mcGeneratorSettings           = ""
  , mcHardcore                    = False
  , mcLevelName                   = "world"
  , mcLevelSeed                   = ""
  , mcLevelType                   = Default
  , mcMaxBuildHeight              = 256
  , mcMaxPlayers                  = 20
  , mcMaxTickTime                 = Just 60000
  , mcMaxWorldSize                = 29999984
  , mcMotd                        = "A Minecraft Server"
  , mcNetworkCompressionThreshold = Nothing
  , mcOnlineMode                  = False
  , mcOpPermissionLevel           = 4
  , mcPlayerIdleTimeout           = 0
  , mcPvp                         = True
  , mcQueryPort                   = 25565
  , mcRconPassword                = ""
  , mcResourcePack                = ""
  , mcResourcePackHash            = ""
  , mcServerIP                    = ""
  , mcServerPort                  = 25565
  , mcSnooperEnabled              = True
  , mcSpawnAnimals                = True
  , mcSpawnMonsters               = True
  , mcSpawnNPCs                   = True
  , mcSpawnProtection             = 16
  , mcUseNativeTransport          = True
  , mcViewDistance                = 10
  , mcWhiteList                   = False }


