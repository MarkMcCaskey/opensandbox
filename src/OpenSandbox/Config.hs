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
  , debugConfig
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
import qualified  Data.Text as T
import            Data.Word
import            Data.X509
import            Data.Yaml
import            OpenSandbox.Types
import            OpenSandbox.Version

data Config = Config
  { srvPort             :: Int
  , srvPath             :: FilePath
  , srvConfigPath       :: FilePath
  , srvBackupPath       :: FilePath
  , srvLogPath          :: FilePath
  , srvWorldPath        :: FilePath
  , srvMCVersion        :: T.Text
  , srvMaxPlayers       :: Word8
  , srvGameMode         :: GameMode
  , srvDifficulty       :: Difficulty
  , srvWorldType        :: WorldType
  , srvMotd             :: T.Text
  , srvEnabled          :: Bool
  } deriving (Show,Eq)


instance FromJSON Config where
  parseJSON (Object v) =
    Config  <$> v .: "port"
            <*> v .: "rootPath"
            <*> v .: "configPath"
            <*> v .: "backupPath"
            <*> v .: "logPath"
            <*> v .: "worldPath"
            <*> v .: "mcVersion"
            <*> v .: "maxPlayers"
            <*> v .: "gameMode"
            <*> v .: "difficulty"
            <*> v .: "worldType"
            <*> v .: "motd"
            <*> v .: "enabled"
  parseJSON _ = mzero


debugConfig :: Config
debugConfig = Config
  { srvPort = 25567
  , srvPath = "."
  , srvConfigPath = "config"
  , srvBackupPath = "backup"
  , srvLogPath = "logs"
  , srvWorldPath = "world"
  , srvMCVersion = snapshotVersion
  --, srvPlayerCount = 0
  , srvMaxPlayers = 20
  , srvGameMode = Survival
  --, srvDimension = Overworld
  , srvDifficulty = Normal
  , srvWorldType = Default
  , srvMotd = "A OpenSandbox Server"
  --, srvEncryption = Nothing
  --, srvCompression = Nothing
  , srvEnabled = False
  }

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = decodeFile path

configEncryption :: IO (Maybe Encryption)
configEncryption = do
  (pubKey,privKey) <- generate 128 65537
  let cert = encodeASN1' DER $ toASN1 (PubKeyRSA pubKey) []
  return (Just (Encryption cert pubKey privKey (B.pack [26,120,188,217])))
