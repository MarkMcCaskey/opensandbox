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
  , writeDefaultConfig
  , genDefaultConfig
  , configEncryption
  , loadConfig
  ) where

import            Control.Monad
import            Crypto.PubKey.RSA
import            Data.Aeson
import            Data.ASN1.BinaryEncoding
import            Data.ASN1.Encoding
import            Data.ASN1.Types hiding (End)
import qualified  Data.ByteString as B
import            Data.Int
import qualified  Data.Text as T
import            Data.Word
import            Data.X509
import            Data.Yaml
import            OpenSandbox.Types
import            OpenSandbox.Version
import            Path


data Config = Config
  { srvPort             :: Int
  , srvRootDir          :: (Path Abs Dir)
  , srvConfigDir        :: (Path Rel Dir)
  , srvBackupDir        :: (Path Rel Dir)
  , srvLogDir           :: (Path Rel Dir)
  , srvWorldDir         :: (Path Rel Dir)
  , srvMCVersion        :: T.Text
  , srvMaxPlayers       :: Int32
  , srvViewDistance     :: Word8
  , srvMaxBuildHeight   :: Int
  , srvGameMode         :: GameMode
  , srvDimension        :: Dimension
  , srvDifficulty       :: Difficulty
  , srvWorldType        :: WorldType
  , srvMotd             :: T.Text
  , srvEncryption       :: Bool
  , srvCompression      :: Bool
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
            if (srvMaxPlayers c < (maxBound :: Int32)) && (srvMaxPlayers c >= 0)
              then Right c
              else Left "Error: Invalid Max Players!"

      return $ (hasValidViewDistance >=> hasValidMaxBuildHeight >=> hasValidMaxPlayers) rawConfig


configEncryption :: IO Encryption
configEncryption = do
  (pubKey,privKey) <- generate 128 65537
  let cert = encodeASN1' DER $ toASN1 (PubKeyRSA pubKey) []
  return (Encryption cert pubKey privKey (B.pack [26,120,188,217]))
