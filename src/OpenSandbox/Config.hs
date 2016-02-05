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
  ) where

import            Crypto.PubKey.RSA
import            Data.ASN1.BinaryEncoding
import            Data.ASN1.Encoding
import            Data.ASN1.Types hiding (End)
import qualified  Data.ByteString as B
import            Data.X509
import            OpenSandbox.Logger
import            OpenSandbox.Types
import            OpenSandbox.Version


data Config = Config
  { srvPort             :: Int
  , srvPath             :: FilePath
  , srvConfigPath       :: FilePath
  , srvBackupPath       :: FilePath
  , srvLogPath          :: FilePath
  , srvWorldPath        :: FilePath
  , srvMCVersion        :: String
  , srvPlayerCount      :: Int
  , srvMaxPlayers       :: Int
  , srvGameMode         :: GameMode
  , srvDimension        :: Dimension
  , srvDifficulty       :: Difficulty
  , srvWorldType        :: WorldType
  , srvMotd             :: String
  , srvEncryption       :: Maybe Encryption
  , srvCompression      :: Maybe Compression
  , srvEnabled          :: Bool
  , srvUp               :: Bool
  } deriving (Show,Eq)

debugConfig :: Config
debugConfig = Config
  { srvPort = 25567
  , srvPath = "."
  , srvConfigPath = "config"
  , srvBackupPath = "backup"
  , srvLogPath = "logs"
  , srvWorldPath = "world"
  , srvMCVersion = snapshotVersion
  , srvPlayerCount = 0
  , srvMaxPlayers = 20
  , srvGameMode = Survival
  , srvDimension = Overworld
  , srvDifficulty = Normal
  , srvWorldType = Default
  , srvMotd = "A OpenSandbox Server"
  , srvEncryption = Nothing
  , srvCompression = Nothing
  , srvEnabled = False
  , srvUp = True
  }

configEncryption :: IO (Maybe Encryption)
configEncryption = do
  (pubKey,privKey) <- generate 128 65537
  let cert = encodeASN1' DER $ toASN1 (PubKeyRSA pubKey) []
  return (Just (Encryption cert pubKey privKey (B.pack [26,120,188,217])))
