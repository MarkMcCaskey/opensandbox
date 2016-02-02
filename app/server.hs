{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- |
-- File         : server.hs
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------

import            Control.Monad
import            Control.Monad.Catch
import            Control.Monad.IO.Class
import            Control.Monad.Trans.Class
import            Control.Monad.Trans.State.Lazy
import qualified  Data.Aeson as Aeson
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Char8 as BC
import qualified  Data.ByteString.Lazy as BL
import            Data.Conduit
import            Data.Conduit.Cereal
import            Data.Conduit.Network
import qualified  Data.Serialize as S
import            Data.Text.Encoding
import            Data.UUID
import            Data.UUID.V4
import            OpenSandbox
import            System.Log.FastLogger

myVersion :: String
myVersion = "16w04a"

myVersionID :: Int
myVersionID = 97

myBackupPath :: FilePath
myBackupPath = "backup"

myLogPath :: FilePath
myLogPath = "log"

mySrvPath :: FilePath
mySrvPath = "."

myPort :: Int
myPort = 25567

main :: IO ()
main = do
    let port = myPort
    logger <- newStdoutLoggerSet defaultBufSize
    writeTo logger Info "Welcome to the OpenSandbox Minecraft Server!"
    let config = defaultConfig
    writeTo logger Info "Reading server configs..."
    maybeEncryption <- configEncryption config logger
    maybeCompression <- configCompression config logger
    writeTo logger Info $ "Starting minecraft server version " ++ show myVersion
    writeTo logger Info $ "Default game type: " ++ show (mcLevelType config)
    writeTo logger Info $ "Starting Minecraft server on " ++ show port
    writeTo logger Info $ "Preparing level " ++ show (mcLevelName config)
    writeTo logger Info $ "Done!"
    let currentPlayers = 0
    let srv = Server
                { srvName = "Opensandbox"
                , srvPort = myPort
                , srvPath = mySrvPath
                , srvBackupPath = myBackupPath
                , srvLogPath = myLogPath
                , srvWorld = "world"
                , srvVersion = myVersion
                , srvPlayers = currentPlayers
                , srvMaxPlayers = (mcMaxPlayers config)
                , srvMotd = (mcMotd config)
                , srvEncryption = maybeEncryption
                , srvCompression = maybeCompression
                , srvEnabled = False
                , srvUp = False
                }
    runTCPServer (serverSettings myPort "*") $ runOpenSandbox srv logger

runOpenSandbox :: Server -> LoggerSet -> AppData -> IO ()
runOpenSandbox srv logger app = do
    protocolState <- flip execStateT Handshake
      $ packetSource app
      $$ deserializeStatus
      =$= handleStatus srv logger
      =$= serializeStatus
      =$= packetSink app
    writeTo logger Debug "Somebody's pinging!"
    if protocolState == Login
      then do
        writeTo logger Debug "Somebody's logging in!"
        nextState <- flip execStateT Login
          $ packetSource app
          $$ deserializeLogin
          =$= handleLogin srv logger
          =$= serializeLogin
          =$= packetSink app
        if nextState == Play
          then do
            writeTo logger Debug "Somebody succeeded to login"
            void $ flip execStateT Play
              $ packetSource app
              $$ deserializePlay
              =$= handlePlay srv logger
              =$= serializePlay
              =$= packetSink app
          else writeTo logger Debug "Somebody failed login"
      else return ()

packetSource :: AppData -> Source (StateT ProtocolState IO) B.ByteString
packetSource app = transPipe lift $ appSource app

packetSink :: AppData -> Sink B.ByteString (StateT ProtocolState IO) ()
packetSink app = transPipe lift $ appSink app

deserializeStatus :: Conduit B.ByteString (StateT ProtocolState IO) ServerBoundStatus
deserializeStatus = conduitGet (S.get :: S.Get ServerBoundStatus)

serializeStatus :: Conduit ClientBoundStatus (StateT ProtocolState IO) B.ByteString
serializeStatus = conduitPut (S.put :: S.Putter ClientBoundStatus)

deserializeLogin :: Conduit B.ByteString (StateT ProtocolState IO) ServerBoundLogin
deserializeLogin = conduitGet (S.get :: S.Get ServerBoundLogin)

serializeLogin :: Conduit ClientBoundLogin (StateT ProtocolState IO) B.ByteString
serializeLogin = conduitPut (S.put :: S.Putter ClientBoundLogin)

deserializePlay :: Conduit B.ByteString (StateT ProtocolState IO) ServerBoundPlay
deserializePlay = conduitGet (S.get :: S.Get ServerBoundPlay)

serializePlay :: Conduit ClientBoundPlay (StateT ProtocolState IO) B.ByteString
serializePlay = conduitPut (S.put :: S.Putter ClientBoundPlay)

handleStatus :: Server -> LoggerSet -> Conduit ServerBoundStatus (StateT ProtocolState IO) ClientBoundStatus
handleStatus srv logger = do
  maybeHandshake <- await
  liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeHandshake
  case maybeHandshake of
    Just (ServerBoundHandshake _ _ _ 1) -> do
      maybePingStart <- await
      liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybePingStart
      lift $ put Status
      let version = srvVersion srv
      let versionID = myVersionID
      let players = srvPlayers srv
      let maxPlayers = srvMaxPlayers srv
      let motd = srvMotd srv
      let responsePacket = ClientBoundResponse . BL.toStrict . Aeson.encode
            $ buildStatus version versionID players maxPlayers motd
      liftIO $ writeTo logger Debug $ "Sending: " ++ show responsePacket
      yield responsePacket
      maybePing <- await
      liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybePing
      case maybePing of
        Just (ServerBoundPing payload) -> do
          let pong = ClientBoundPong payload
          liftIO $ writeTo logger Debug $ "Sending: " ++ show pong
          yield pong
        Nothing -> return ()
    Just (ServerBoundHandshake _ _ _ 2) -> do
      liftIO $ writeTo logger Debug $ "Switching protocol state to LOGIN"
      lift $ put Login
    Just _ -> return ()
    Nothing -> return ()

handleLogin :: Server -> LoggerSet -> Conduit ServerBoundLogin (StateT ProtocolState IO) ClientBoundLogin
handleLogin srv logger = do
  maybeLoginStart <- await
  liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeLoginStart
  case maybeLoginStart of
    Just (ServerBoundLoginStart username) ->
      do  someUUID <- liftIO $ nextRandom
          let loginSuccess = ClientBoundLoginSuccess (BC.pack $ show someUUID) username
          liftIO $ writeTo logger Debug $ "Sending: " ++ show loginSuccess
          yield loginSuccess
          liftIO $ writeTo logger Debug $ "Switching protocol state to PLAY"
          lift $ put Play
    Just _ -> return ()
    Nothing -> return ()

handlePlay :: Server -> LoggerSet -> Conduit ServerBoundPlay (StateT ProtocolState IO) ClientBoundPlay
handlePlay srv logger = do
  liftIO $ writeTo logger Debug $ "Starting PLAY session"
  yield $ login 2566 Survival Overworld Normal 20 Default True
  --yield $ channel
  yield $ difficulty Normal
  yield $ updateTime 0 0
  yield $ abilities 0 0 0
  yield $ heldItemSlot 0
  --yield $ explosion
  --yield $ statistics
  --yield $ playerInfo
