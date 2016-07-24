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

    rootDir <-
      case getCustomRootDir args of
        Nothing -> parseAbsDir =<< getCurrentDirectory
        Just customRootDir -> parseAbsDir customRootDir

    configFilePath <-
      case getCustomConfigDir args of
        Nothing -> do
          configDir <- parseRelDir "config"
          configFile <- parseRelFile "opensandboxd.yaml"
          createDirectoryIfMissing True $ toFilePath $ rootDir </> configDir
          return $ rootDir </> configDir </> configFile
        Just customPath -> do
          configDir <- parseRelDir customPath
          configFile <- parseRelFile "opensandboxd.yaml"
          createDirectoryIfMissing True $ toFilePath $ rootDir </> configDir
          return $ rootDir </> configDir </> configFile

    configFileExists <- doesFileExist (toFilePath configFilePath)
    if configFileExists
      then return ()
      else genDefaultConfig (toFilePath configFilePath)

    potentialConfig <- loadConfig configFilePath
    case potentialConfig of
      Left err -> print err
      Right config -> do
        encryption <- configEncryption
        logFilePath <-
          case getCustomLogDir args of
            Nothing -> do
              logDir <- parseRelDir "logs"
              logFile <- parseRelFile "latest.log"
              createDirectoryIfMissing True $ toFilePath $ rootDir </> logDir
              return $ rootDir </> logDir </> logFile
            Just customPath -> do
              logDir <- parseRelDir customPath
              logFile <- parseRelFile "latest.log"
              createDirectoryIfMissing True $ toFilePath $ rootDir </> logDir
              return $ rootDir </> logDir </> logFile
        logger <- newLogger defaultBufSize (toFilePath logFilePath) $
          if getDebugFlag args
            then Debug
            else Info
        writeTo logger Info "----------------- Log Start -----------------"
        writeTo logger Info "Welcome to the OpenSandbox Minecraft Server!"
        writeTo logger Info $ "Starting minecraft server version " ++ show snapshotVersion
        writeTo logger Info $ "Starting Minecraft server on " ++ show (srvPort config)
        writeTo logger Info $ "Done!"
        runOpenSandboxServer config logger encryption
