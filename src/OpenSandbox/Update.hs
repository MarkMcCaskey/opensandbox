{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Update
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Update
    ( findLatestMCSnapshot
    , getLatestMCSnapshot
    , getMCSnapshot
    , mcServerJar
    ) where


import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.List as List
import            GHC.Generics
import            Network.HTTP.Conduit


type MCVersion = String


-- | Grabs the latest Minecraft snapshot from Mojang's servers
-- param cacheDir: The path to the cache directory
getLatestMCSnapshot :: FilePath -> IO ()
getLatestMCSnapshot cacheDir = do
    mcVersion <- findLatestMCSnapshot
    case mcVersion of
      Left err -> putStrLn $ "Error: Could not get latest snapshot. " ++ err
      Right v -> getMCSnapshot cacheDir v


-- | Grabs a Minecraft snapshot from Mojang's servers
-- param version: The version num of the target snapshot
-- param cacheDir: The path to the cache directory
-- returns: An IO action that saves the snapshot of said version to the said cache directory
getMCSnapshot :: FilePath -> String -> IO ()
getMCSnapshot dest version = do
    let url = List.intercalate "/" [mcVersionsURL, version, mcServerJar version]
    simpleHttp url >>= BL.writeFile (dest ++ "/" ++ mcServerJar version)


findLatestMCSnapshot :: IO (Either String MCVersion)
findLatestMCSnapshot = do
    raw <- getMCVersionList
    case raw of
      Left err -> return $ Left err
      Right vList -> return $ Right $ snapshot $ latest vList


getMCVersionList :: IO (Either String VersionList)
getMCVersionList = eitherDecode <$> simpleHttp mcVersionsListURL


mcVersionsURL :: String
mcVersionsURL = "https://s3.amazonaws.com/Minecraft.Download/versions"


mcVersionsListURL :: String
mcVersionsListURL = "https://s3.amazonaws.com/Minecraft.Download/versions/versions.json"


mcServerJar :: MCVersion -> String
mcServerJar version = "minecraft_server." ++ version ++ ".jar"


data VersionList = VersionList
  { latest    :: Latest
  , versions  :: [Version]
  } deriving (Show,Generic)


instance ToJSON VersionList
instance FromJSON VersionList


data Version = Version
  { versionID          :: String
  , versionTime        :: String
  , versionReleaseTime :: String
  , versionType        :: String
  } deriving (Show)


instance ToJSON Version where
  toJSON (Version versionID versionTime versionReleaseTime versionType) =
    object [ "id"           .= versionID
           , "time"         .= versionTime
           , "releaseTime"  .= versionReleaseTime
           , "type"         .= versionType
           ]


instance FromJSON Version where
  parseJSON (Object v) =
    Version <$> v .: "id"
            <*> v .: "time"
            <*> v .: "releaseTime"
            <*> v .: "type"
  parseJSON _ = mzero


data Latest = Lastest
  { snapshot  :: String
  , release   :: String
  } deriving (Show,Generic)


instance ToJSON Latest
instance FromJSON Latest
