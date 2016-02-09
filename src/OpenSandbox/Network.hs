{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Network
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Network
  ( runOpenSandboxServer
  ) where

import            Control.Monad
import            Control.Monad.IO.Class
import            Control.Monad.Trans.Class
import            Control.Monad.Trans.State.Lazy
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Char8 as BC
import            Data.Conduit
import            Data.Conduit.Cereal
import            Data.Conduit.Network
import qualified  Data.Serialize as S
import            Data.UUID.V4

import            OpenSandbox.Config
import            OpenSandbox.Logger
import            OpenSandbox.Protocol
import            OpenSandbox.Types
import            OpenSandbox.Version

runOpenSandboxServer :: Config -> Logger -> IO ()
runOpenSandboxServer config logger =
    runTCPServer (serverSettings (srvPort config) "*") $ \app -> do
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
            =$= handleLogin logger
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

      let statusPacket = statusResponse
                          snapshotVersion
                          protocolVersion
                          0
                          (srvMaxPlayers config)
                          (srvMotd config)
      liftIO $ writeTo logger Debug $ "Sending: " ++ show statusPacket
      yield statusPacket

      maybePing <- await
      liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybePing
      case maybePing of
        Just (ServerBoundPing payload) -> do
          let pongPacket = ClientBoundPong payload
          liftIO $ writeTo logger Debug $ "Sending: " ++ show pongPacket
          yield pongPacket
        Just _ -> liftIO $ writeTo logger Err $ "Expecting ServerBoundPing, recieved " ++ show maybePing
        Nothing -> return ()

    Just (ServerBoundHandshake _ _ _ 2) -> do
      liftIO $ writeTo logger Debug $ "Switching protocol state to LOGIN"
      lift $ put Login
    Just _ -> liftIO $ writeTo logger Err $ "Expecting ServerBoundHandshake, recieved " ++ show maybeHandshake
    Nothing -> return ()


handleLogin :: Logger -> Conduit ServerBoundLogin (StateT ProtocolState IO) ClientBoundLogin
handleLogin logger = do
  maybeLoginStart <- await
  liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeLoginStart
  case maybeLoginStart of
    Just (ServerBoundLoginStart username) ->
      do  someUUID <- liftIO nextRandom
          let loginSuccess = ClientBoundLoginSuccess (BC.pack $ show someUUID) username
          liftIO $ writeTo logger Debug $ "Sending: " ++ show loginSuccess
          yield loginSuccess
          liftIO $ writeTo logger Debug $ "Switching protocol state to PLAY"
          lift $ put Play
    Just _ -> liftIO $ writeTo logger Err $ "Expecting ServerBoundLoginStart, recieved " ++ show maybeLoginStart
    Nothing -> return ()


handlePlay :: Config -> Logger -> Conduit ServerBoundPlay (StateT ProtocolState IO) ClientBoundPlay
handlePlay config logger = do
  liftIO $ writeTo logger Debug $ "Starting PLAY session"

  let loginPacket = login
                      2566
                      (srvGameMode config)
                      Overworld
                      (srvDifficulty config)
                      (srvMaxPlayers config)
                      (srvWorldType config)
                      True
  liftIO $ writeTo logger Debug $ "Sending: " ++ show loginPacket
  yield loginPacket

  let customPayloadPacket1 = customPayload "MC|Brand" "opensandbox"
  liftIO $ writeTo logger Debug $ "Sending: " ++ show customPayloadPacket1
  yield customPayloadPacket1

  let customPayloadPacket2 = customPayload "REGISTER" "MC|Brand"
  liftIO $ writeTo logger Debug $ "Sending: " ++ show customPayloadPacket2
  yield customPayloadPacket2

  let difficultyPacket = difficulty (srvDifficulty config)
  liftIO $ writeTo logger Debug $ "Sending: " ++ show difficultyPacket
  yield difficultyPacket

  let updateTimePacket = updateTime 1000 25
  liftIO $ writeTo logger Debug $ "Sending: " ++ show updateTimePacket
  yield updateTimePacket

  let abilitiesPacket = abilities 0 0 0
  liftIO $ writeTo logger Debug $ "Sending: " ++ show abilitiesPacket
  yield abilitiesPacket

  let heldItemSlotPacket = heldItemSlot 0
  liftIO $ writeTo logger Debug $ "Sending: " ++ show heldItemSlotPacket
  yield heldItemSlotPacket

  --let entityStatusPacket = entityStatus
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show entityStatusPacket
  --yield entityStatusPacket

  let statisticsPacket = statistics []
  liftIO $ writeTo logger Debug $ "Sending: " ++ show statisticsPacket
  yield statisticsPacket

  --let playerInfoPacket = playerInfo
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show playerInfoPacket
  --yield playerInfoPacket
