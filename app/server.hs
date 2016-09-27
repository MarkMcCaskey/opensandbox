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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map.Strict as MS
import OpenSandbox
import Path
import System.Directory
import System.Exit
import qualified System.Remote.Monitoring as Ekg

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

          if getEkgFlag args
            then void $ Ekg.forkServer "localhost" 8000
            else when (srvEkgEnabled config) $ void $ Ekg.forkServer "localhost" 8000

          -- Encryption Step
          encryption <- configEncryption
          logMsg logger LvlInfo "Generating keypair..."

          logMsg logger LvlInfo $ "Starting Minecraft server on " ++ show (srvPort config)

          eitherGameData <- loadGameData

          logMsg logger LvlInfo "Generating world..."
          eitherWorld <- atomically $ newWorld Flat config

          -- User Store
          existingUsers <- newTVarIO MS.empty

          history <- newTVarIO []

          case eitherGameData of
            Left err -> print err
            Right gameData ->
              case eitherWorld of
                Left err -> print err
                Right world -> do
                  worldClock <- newWorldClock
                  _ <- forkIO $ tick worldClock
                  let server = Server config logger worldClock world existingUsers history
                  runOpenSandboxServer server encryption
