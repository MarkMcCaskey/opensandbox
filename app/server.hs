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

import            Control.Monad
import qualified  Data.Aeson as A
import qualified  Data.ByteString as B
import            Data.Maybe
import qualified  Data.Text as T
import            OpenSandbox
import            Path
import            System.Directory
import            System.Exit

logMsg :: Logger -> Lvl -> String -> IO ()
logMsg logger lvl msg = logIO logger "Main" lvl (T.pack msg)

main :: IO ()
main = do
    args <- getOpts
    when (getVersionFlag args) $ do
      print $ "OpenSandbox Version:   " `T.append` openSandboxVersion
      print $ "MC Stable Version:     " `T.append` majorVersion
      print $ "MC Snapshot Version:   " `T.append` snapshotVersion
      exitSuccess

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
          return (rootDir </> configDir </> configFile, config1)
        Just customPath -> do
          configDir <- parseRelDir customPath
          configFile <- parseRelFile "opensandboxd.yaml"
          createDirectoryIfMissing True $ toFilePath $ rootDir </> configDir
          return (rootDir </> configDir </> configFile, config1 {srvConfigDir = configDir})

    configFileExists <- doesFileExist (toFilePath configFilePath)
    unless configFileExists $ writeDefaultConfig (toFilePath configFilePath) config2


    potentialConfig <- loadConfig configFilePath
    case potentialConfig of
        Left err -> print err
        Right baseConfig -> do

          (logFilePath,config) <-
            case getCustomLogDir args of
              Nothing -> do
                logDir <- parseRelDir "logs"
                logFile <- parseRelFile "latest.log"
                createDirectoryIfMissing True $ toFilePath $ rootDir </> logDir
                return (rootDir </> logDir </> logFile,baseConfig)
              Just customPath -> do
                logDir <- parseRelDir customPath
                logFile <- parseRelFile "latest.log"
                createDirectoryIfMissing True $ toFilePath $ rootDir </> logDir
                return (rootDir </> logDir </> logFile,baseConfig {srvLogDir = logDir})

          let spec = FileLogSpec (toFilePath logFilePath) 1000000 10

          logger <- newLogger spec $
            if getDebugFlag args
              then LvlDebug
              else LvlInfo

          _ <- runLogger logger
          logMsg logger LvlInfo "----------------- Log Start -----------------"
          logMsg logger LvlInfo "Welcome to the OpenSandbox Minecraft Server!"
          logMsg logger LvlInfo $ "Starting minecraft server version " ++ show snapshotVersion
          logMsg logger LvlInfo $ "Loading config from: " ++ show (srvConfigDir config)
          logMsg logger LvlInfo $ "Starting OpenSandbox server at: " ++ show (srvRootDir config)
          logMsg logger LvlInfo $ "Writing logs to: " ++ show (srvLogDir config)

          -- Encryption Step
          encryption <- configEncryption
          logMsg logger LvlInfo "Generating keypair..."

          logMsg logger LvlInfo $ "Starting Minecraft server on " ++ show (srvPort config)

          rawBiomes <- B.readFile "data/biomes.json"
          rawBlocks <- B.readFile "data/blocks.json"
          rawEffects <- B.readFile "data/effects.json"
          rawEntity <- B.readFile "data/entities.json"
          rawInstruments <- B.readFile "data/instruments.json"
          rawItems <- B.readFile "data/items.json"

          let !biomes = A.eitherDecodeStrict' rawBiomes :: Either String [Biome]
          let !blocks = A.eitherDecodeStrict' rawBlocks :: Either String [BlockImport]
          let !effects = A.eitherDecodeStrict' rawEffects :: Either String [Effect]
          let !entities = A.eitherDecodeStrict' rawEntity :: Either String [Entity]
          let !instruments = A.eitherDecodeStrict' rawInstruments :: Either String [Instrument]
          let !items = A.eitherDecodeStrict' rawItems :: Either String [Item]

          --let !globalPalette = fmap mkGlobalPalette blocks

          -- World Gen Step
          --logMsg logger LvlInfo "Generating world..."
          --world <- genWorld logger

          runOpenSandboxServer config logger encryption
