{-# LANGUAGE OverloadedStrings #-}
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
                return $ (rootDir </> logDir </> logFile,config2)
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
          runOpenSandboxServer config logger encryption
