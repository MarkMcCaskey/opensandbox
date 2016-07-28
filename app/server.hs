{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-------------------------------------------------------------------------------
-- |
-- File         : server.hs
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------

import qualified  Data.Aeson as A
import qualified  Data.ByteString as B
import            Data.Maybe
import qualified  Data.Text as T
import            OpenSandbox
import            Path
import            System.Directory
import            System.Exit

main :: IO ()
main = do
    args <- getOpts
    if getVersionFlag args
      then do print $ "OpenSandbox Version:   " `T.append` openSandboxVersion
              print $ "MC Stable Version:     " `T.append` majorVersion
              print $ "MC Snapshot Version:   " `T.append` snapshotVersion
              exitSuccess
      else return ()

    (rootDir,config1) <-
      case getCustomRootDir args of
        Nothing -> do
          rootDir <- parseAbsDir =<< getCurrentDirectory
          config0 <- genDefaultConfig
          return (rootDir,config0 {srvRootDir = rootDir})
        Just customRootDir -> do
          rootDir <- parseAbsDir customRootDir
          config0 <- genDefaultConfig
          return (rootDir,config0 {srvRootDir = rootDir})

    (configFilePath,config2) <-
      case getCustomConfigDir args of
        Nothing -> do
          configDir <- parseRelDir "config"
          configFile <- parseRelFile "opensandboxd.yaml"
          createDirectoryIfMissing True $ toFilePath $ rootDir </> configDir
          return $ (rootDir </> configDir </> configFile, config1)
        Just customPath -> do
          configDir <- parseRelDir customPath
          configFile <- parseRelFile "opensandboxd.yaml"
          createDirectoryIfMissing True $ toFilePath $ rootDir </> configDir
          return $ (rootDir </> configDir </> configFile, config1 {srvConfigDir = configDir})

    configFileExists <- doesFileExist (toFilePath configFilePath)
    if configFileExists
      then return ()
      else writeDefaultConfig (toFilePath configFilePath) config2

    potentialConfig <- loadConfig configFilePath
    case potentialConfig of
        Left err -> print err
        Right baseConfig -> do
          encryption <- configEncryption
          (logFilePath,config) <-
            case getCustomLogDir args of
              Nothing -> do
                logDir <- parseRelDir "logs"
                logFile <- parseRelFile "latest.log"
                createDirectoryIfMissing True $ toFilePath $ rootDir </> logDir
                return $ (rootDir </> logDir </> logFile,baseConfig)
              Just customPath -> do
                logDir <- parseRelDir customPath
                logFile <- parseRelFile "latest.log"
                createDirectoryIfMissing True $ toFilePath $ rootDir </> logDir
                return $ (rootDir </> logDir </> logFile,baseConfig {srvLogDir = logDir})

          logger <- newLogger defaultBufSize (toFilePath logFilePath) $
            if getDebugFlag args
              then Debug
              else Info

          writeTo logger Info "----------------- Log Start -----------------"
          writeTo logger Info "Welcome to the OpenSandbox Minecraft Server!"
          writeTo logger Info $ "Starting minecraft server version " ++ show snapshotVersion
          writeTo logger Info $ "Starting OpenSandbox server at: " ++ show (srvRootDir config)
          writeTo logger Info $ "Loading config from: " ++ show (srvConfigDir config)
          writeTo logger Info $ "Writing logs to: " ++ show (srvLogDir config)
          writeTo logger Info $ "Starting Minecraft server on " ++ show (srvPort config)
          writeTo logger Info $ "Done!"
          {-
          rawBiomes <- B.readFile "data/biomes.json"
          rawBlocks <- B.readFile "data/blocks.json"
          rawEffects <- B.readFile "data/effects.json"
          rawEntity <- B.readFile "data/entities.json"
          rawInstruments <- B.readFile "data/instruments.json"
          rawItems <- B.readFile "data/items.json"

          let !biomes = A.eitherDecodeStrict' rawBiomes :: Either String [Biome]
          let !blocks = A.eitherDecodeStrict' rawBlocks :: Either String [Block]
          let !effects = A.eitherDecodeStrict' rawEffects :: Either String [Effect]
          let !entities = A.eitherDecodeStrict' rawEntity :: Either String [Entity]
          let !instruments = A.eitherDecodeStrict' rawInstruments :: Either String [Instrument]
          let !items = A.eitherDecodeStrict' rawItems :: Either String [Item]
          -}
          runOpenSandboxServer config logger encryption
