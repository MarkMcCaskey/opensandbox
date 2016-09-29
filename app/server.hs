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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as B
import Data.Conduit
import Data.Conduit.Network
import qualified Data.Text as T
import qualified Data.Map.Strict as MS
import OpenSandbox
import Path
import System.Directory
import System.Exit
import qualified System.Remote.Monitoring as Ekg

logMsg :: Logger -> Lvl -> String -> IO ()
logMsg logger lvl msg = logIO logger "Server" lvl (T.pack msg)

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

runOpenSandboxServer :: Server -> Encryption -> IO ()
runOpenSandboxServer server encryption =
    runTCPServer (serverSettings (srvPort config) "*") $ \app -> do
      firstState <- flip execStateT ProtocolHandshake
        $ packetSource app
        $$ deserializeHandshaking
        =$= handleHandshaking logger
        =$= handleStatus config logger
        =$= serializePacket
        =$= fmtPacket
        =$= packetSink app
      liftIO $ logMsg logger LvlDebug $ "Somebody's handshaking!"
      case firstState of
        ProtocolStatus -> do
          liftIO $ logMsg logger LvlDebug $ "Beginning Status handling..."
          _ <- flip execStateT ProtocolStatus
            $ packetSource app
            $$ breakupPackets
            =$= deserializePacket
            =$= handleStatus config logger
            =$= serializePacket
            =$= fmtPacket
            =$= packetSink app
          liftIO $ logMsg logger LvlDebug $ "Somebody's pinging!"
          _ <- flip execStateT ProtocolStatus
            $ packetSource app
            $$ breakupPackets
            =$= deserializePacket
            =$= handleStatus config logger
            =$= serializePacket
            =$= fmtPacket
            =$= packetSink app
          return ()
        ProtocolLogin -> do
          liftIO $ logMsg logger LvlDebug $ "Beginning Login handling..."
          let freshSession = Session
                { sessionProtoState = ProtocolLogin
                , sessionUsername = Nothing
                , sessionSharedSecret = Nothing
                , sessionVerifyToken = (getVerifyToken encryption)
                , sessionCompressionIsEnabled = (srvCompression config)
                , sessionEncryptionIsEnabled = (srvEncryption config)
                , sessionCompressionIsActive = False
                , sessionEncryptionIsActive = False
                }
          session <- flip execStateT freshSession
            $ packetSource app
            $$ breakupPackets
            =$= deserializePacket
            =$= handleLogin logger encryption existingUsers
            =$= serializeLogin
            =$= packetSink app
          liftIO $ logMsg logger LvlDebug $ show session
          if (sessionProtoState session) == ProtocolPlay
            then do
              liftIO $ logMsg logger LvlDebug $ "Beginning Play handling..."
              void $ flip execStateT session
                $ packetSource app
                $$ decryptPacket
                =$= breakupPackets
                =$= decompressPacket
                =$= deserializePacket
                =$= handlePlay config logger worldClock world journal existingUsers
                =$= serializePacket
                =$= compressPacket
                =$= fmtPacket
                =$= encryptPacket
                =$= packetSink app
            else liftIO $ logMsg logger LvlDebug $ "Somebody failed login"
        _ -> return ()
  where
    config = srvConfig server
    logger = srvLogger server
    worldClock = srvWorldClock server
    world = srvWorld server
    existingUsers = srvUserCache server
    journal = srvEventJournal server

packetSource  :: MonadIO m => AppData -> Source m B.ByteString
packetSource app = transPipe liftIO $ appSource app

packetSink  :: MonadIO m => AppData -> Sink B.ByteString m ()
packetSink app = transPipe liftIO $ appSink app

