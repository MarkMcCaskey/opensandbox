{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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
  ( WorldType (..)
  , Config (..)
  , writeDefaultConfig
  , genDefaultConfig
  , loadConfig
  ) where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import Data.Word
import Data.Yaml
import GHC.Generics (Generic)
import OpenSandbox.Protocol.Types
import Path

data WorldType = Default | Flat | LargeBiomes | Amplified
  deriving (Eq,Enum,Generic)

instance Show WorldType where
  show Default = "default"
  show Flat = "flat"
  show LargeBiomes = "largeBiomes"
  show Amplified = "amplified"

instance ToJSON WorldType
instance FromJSON WorldType

data Config = Config
  { srvPort             :: Int
  , srvRootDir          :: (Path Abs Dir)
  , srvConfigDir        :: (Path Rel Dir)
  , srvBackupDir        :: (Path Rel Dir)
  , srvLogDir           :: (Path Rel Dir)
  , srvWorldDir         :: (Path Rel Dir)
  , srvMCVersion        :: T.Text
  , srvMaxPlayers       :: Word8
  , srvViewDistance     :: Word8
  , srvMaxBuildHeight   :: Int
  , srvGameMode         :: GameMode
  , srvDimension        :: Dimension
  , srvDifficulty       :: Difficulty
  , srvWorldType        :: WorldType
  , srvMotd             :: T.Text
  , srvEncryption       :: Bool
  , srvCompression      :: Bool
  , srvEkgEnabled       :: Bool
  , srvEnabled          :: Bool
  } deriving (Show,Eq)

instance ToJSON Config where
  toJSON c = object
    [ "port"            .= srvPort c
    , "rootDir"         .= srvRootDir c
    , "configDir"       .= srvConfigDir c
    , "backupDir"       .= srvBackupDir c
    , "logDir"          .= srvLogDir c
    , "worldDir"        .= srvWorldDir c
    , "mcVersion"       .= srvMCVersion c
    , "maxPlayers"      .= srvMaxPlayers c
    , "viewDistance"    .= srvViewDistance c
    , "maxBuildHeight"  .= srvMaxBuildHeight c
    , "gameMode"        .= srvGameMode c
    , "dimension"       .= srvDimension c
    , "difficulty"      .= srvDifficulty c
    , "worldType"       .= srvWorldType c
    , "motd"            .= srvMotd c
    , "encryption"      .= srvEncryption c
    , "compression"     .= srvCompression c
    , "ekgEnabled"      .= srvEkgEnabled c
    , "enabled"         .= srvEnabled c
    ]

instance FromJSON Config where
  parseJSON (Object v) =
    Config
      <$> v .: "port"
      <*> v .: "rootDir"
      <*> v .: "configDir"
      <*> v .: "backupDir"
      <*> v .: "logDir"
      <*> v .: "worldDir"
      <*> v .: "mcVersion"
      <*> v .: "maxPlayers"
      <*> v .: "viewDistance"
      <*> v .: "maxBuildHeight"
      <*> v .: "gameMode"
      <*> v .: "dimension"
      <*> v .: "difficulty"
      <*> v .: "worldType"
      <*> v .: "motd"
      <*> v .: "encryption"
      <*> v .: "compression"
      <*> v .: "ekgEnabled"
      <*> v .: "enabled"
  parseJSON _ = mzero

writeDefaultConfig :: FilePath -> Config -> IO ()
writeDefaultConfig path c = encodeFile path c

genDefaultConfig :: IO Config
genDefaultConfig = do
  defaultRootDir <- parseAbsDir "/srv/opensandbox"
  defaultConfigDir <- parseRelDir "configs"
  defaultBackupDir <- parseRelDir "backups"
  defaultLogDir <- parseRelDir "logs"
  defaultWorldDir <- parseRelDir "world"
  return $
    Config
      { srvPort             = 25565
      , srvRootDir          = defaultRootDir
      , srvConfigDir        = defaultConfigDir
      , srvBackupDir        = defaultBackupDir
      , srvLogDir           = defaultLogDir
      , srvWorldDir         = defaultWorldDir
      , srvMCVersion        = "1.10"
      , srvMaxPlayers       = 20
      , srvViewDistance     = 10
      , srvMaxBuildHeight   = 256
      , srvGameMode         = Survival
      , srvDimension        = Overworld
      , srvDifficulty       = Normal
      , srvWorldType        = Flat
      , srvMotd             = "A OpenSandbox Server"
      , srvEncryption       = False
      , srvCompression      = False
      , srvEkgEnabled       = False
      , srvEnabled          = False
      }

loadConfig :: Path b File -> IO (Either String Config)
loadConfig path = do
  maybeConfig <- decodeFileEither (toFilePath path)
  case maybeConfig of
    Left err -> return $ Left (show err)
    Right rawConfig -> do

      let hasValidViewDistance c =
            if ((srvViewDistance c) > 2) && ((srvViewDistance c) < 16)
              then Right c
              else Left "Error: Invalid View Distance!"

      let hasValidMaxBuildHeight c =
            if srvMaxBuildHeight c <= 256
              then Right c
              else Left "Error: Invalid Max Build Height!"

      let hasValidMaxPlayers c =
            if (srvMaxPlayers c < (maxBound :: Word8)) && (srvMaxPlayers c >= 0)
              then Right c
              else Left "Error: Invalid Max Players!"

      return $ (hasValidViewDistance >=> hasValidMaxBuildHeight >=> hasValidMaxPlayers) rawConfig
