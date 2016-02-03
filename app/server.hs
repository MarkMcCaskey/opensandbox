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
import            Data.Maybe
import qualified  Data.Serialize as S
import            Data.Text.Encoding
import            Data.UUID
import            Data.UUID.V4
import            OpenSandbox
import            System.Directory


main :: IO ()
main = do
    args <- getOpts opensandboxOpts
    let config = debugConfig
    -- Start Logger
    createDirectoryIfMissing True (srvPath config ++ "/" ++ srvLogPath config)
    let logFilePath =  srvPath config ++ "/" ++ srvLogPath config ++ "/" ++ "latest.log"
    logger <- newLogger defaultBufSize logFilePath (fromJust args)
    writeTo logger Info "----------------- Log Start -----------------"
    writeTo logger Info "Welcome to the OpenSandbox Minecraft Server!"
    writeTo logger Info $ "Starting minecraft server version " ++ show (srvMCVersion config)
    writeTo logger Info $ "Starting Minecraft server on " ++ show (srvPort config)
    writeTo logger Info $ "Done!"
    runTCPServer (serverSettings (srvPort config) "*") $ runOpenSandbox config logger


runOpenSandbox :: Config -> Logger -> AppData -> IO ()
runOpenSandbox config logger app = do
    protocolState <- flip execStateT Handshake
      $ packetSource app
      $$ deserializeStatus
      =$= handleStatus config logger
      =$= serializeStatus
      =$= packetSink app
    writeTo logger Debug "Somebody's pinging!"
    if protocolState == Login
      then do
        writeTo logger Debug "Somebody's logging in!"
        nextState <- flip execStateT Login
          $ packetSource app
          $$ deserializeLogin
          =$= handleLogin config logger
          =$= serializeLogin
          =$= packetSink app
        if nextState == Play
          then do
            writeTo logger Debug "Somebody succeeded to login"
            void $ flip execStateT Play
              $ packetSource app
              $$ deserializePlay
              =$= handlePlay config logger
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


handleStatus :: Config -> Logger -> Conduit ServerBoundStatus (StateT ProtocolState IO) ClientBoundStatus
handleStatus config logger = do
  maybeHandshake <- await
  liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeHandshake
  case maybeHandshake of
    Just (ServerBoundHandshake _ _ _ 1) -> do
      maybePingStart <- await
      liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybePingStart
      lift $ put Status
      let version = snapshotVersion
      let versionID = protocolVersion
      let players = srvPlayerCount config
      let maxPlayers = srvMaxPlayers config
      let motd = srvMotd config
      liftIO $ writeTo logger Debug $ "Sending: " ++ show (statusResponse version versionID players maxPlayers motd)
      yield $ statusResponse version versionID players maxPlayers motd
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


handleLogin :: Config -> Logger -> Conduit ServerBoundLogin (StateT ProtocolState IO) ClientBoundLogin
handleLogin config logger = do
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


handlePlay :: Config -> Logger -> Conduit ServerBoundPlay (StateT ProtocolState IO) ClientBoundPlay
handlePlay config logger = do
  liftIO $ writeTo logger Debug $ "Starting PLAY session"
  yield $ login 2566 Survival Overworld Normal 20 Default True
  yield $ customPayload "MC| Brand" "vanilla"
  yield $ difficulty Normal
  yield $ updateTime 1000 25
  yield $ abilities 0 0 0
  yield $ heldItemSlot 0
  --yield $ explosion
  --yield $ statistics
  --yield $ playerInfo
