{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Config
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Config
  ( Config (..)
  , defaultConfig
  , configEncryption
  , configCompression
  ) where

import            Crypto.PubKey.RSA
import            Data.ASN1.BinaryEncoding
import            Data.ASN1.Encoding
import            Data.ASN1.Types hiding (End)
import qualified  Data.ByteString as B
import            Data.X509
import            OpenSandbox.Logger
import            OpenSandbox.Types


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
  , mcGeneratorSettings           :: !String
  , mcHardcore                    :: !Bool
  , mcLevelName                   :: !String
  , mcLevelSeed                   :: !String
  , mcLevelType                   :: !LevelType
  , mcMaxBuildHeight              :: !Int
  , mcMaxPlayers                  :: !Int
  , mcMaxTickTime                 :: !(Maybe Int)
  , mcMaxWorldSize                :: !Int
  , mcMotd                        :: !String
  , mcNetworkCompressionThreshold :: !(Maybe Compression)
  , mcOnlineMode                  :: !Bool
  , mcOpPermissionLevel           :: !Int
  , mcPlayerIdleTimeout           :: !Int
  , mcPvp                         :: !Bool
  , mcQueryPort                   :: !Int
  , mcRconPassword                :: !String
  , mcResourcePack                :: !String
  , mcResourcePackHash            :: !String
  , mcServerIP                    :: !String
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


configEncryption :: Config -> Logger -> IO (Maybe Encryption)
configEncryption config logger =
  if mcOnlineMode config == True
    then do
      writeTo logger Info "Encryption: [ENABLED]"
      writeTo logger Info "Generating key pair"
      (pubKey,privKey) <- generate 128 65537
      let cert = encodeASN1' DER $ toASN1 (PubKeyRSA pubKey) []
      return (Just (Encryption cert pubKey privKey (B.pack [26,120,188,217])))
    else do
      writeTo logger Info "Encryption: [DISABLED]"
      return Nothing


configCompression :: Config -> Logger -> IO (Maybe Compression)
configCompression config logger =
  if mcNetworkCompressionThreshold config /= Nothing
    then do
      writeTo logger Info "Compression: [ENABLED]"
      return (mcNetworkCompressionThreshold config)
    else do
      writeTo logger Info "Compression: [DISABLED]" >> return Nothing
