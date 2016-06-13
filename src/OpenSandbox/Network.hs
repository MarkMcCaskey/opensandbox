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
import qualified  Data.Vector as V

import            OpenSandbox.Config
import            OpenSandbox.Logger
import            OpenSandbox.Protocol
import            OpenSandbox.Types
import            OpenSandbox.Version

runOpenSandboxServer :: Config -> Logger -> IO ()
runOpenSandboxServer config logger =
    runTCPServer (serverSettings (srvPort config) "*") $ \app -> do
      firstState <- flip execStateT Handshake
        $ packetSource app
        $$ deserializeHandshaking
        =$= handleHandshaking config logger
        =$= serializeStatus
        =$= packetSink app
      writeTo logger Debug "Somebody's pinging!"
      if firstState == Status
        then do
          secondState <- flip execStateT Status
            $ packetSource app
            $$ deserializeStatus
            =$= handleStatus config logger
            =$= serializeStatus
            =$= packetSink app
          writeTo logger Debug "Somebody's pinging!"
          if secondState == Login
            then do
              writeTo logger Debug "Somebody's logging in!"
              thirdState <- flip execStateT Login
                $ packetSource app
                $$ deserializeLogin
                =$= handleLogin logger
                =$= serializeLogin
                =$= packetSink app
              if thirdState == Play
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
        else return ()


packetSource :: AppData -> Source (StateT ProtocolState IO) B.ByteString
packetSource app = transPipe lift $ appSource app


packetSink :: AppData -> Sink B.ByteString (StateT ProtocolState IO) ()
packetSink app = transPipe lift $ appSink app


deserializeHandshaking :: Conduit B.ByteString (StateT ProtocolState IO) SBHandshaking
deserializeHandshaking = undefined -- conduitGet (S.get :: S.Get ServerBoundHandshaking)


deserializeStatus :: Conduit B.ByteString (StateT ProtocolState IO) SBStatus
deserializeStatus = undefined -- conduitGet (S.get :: S.Get ServerBoundStatus)


serializeStatus :: Conduit CBStatus (StateT ProtocolState IO) B.ByteString
serializeStatus = undefined -- conduitPut (S.put :: S.Putter ClientBoundStatus)


deserializeLogin :: Conduit B.ByteString (StateT ProtocolState IO) SBLogin
deserializeLogin = undefined -- conduitGet (S.get :: S.Get ServerBoundLogin)


serializeLogin :: Conduit CBLogin (StateT ProtocolState IO) B.ByteString
serializeLogin = undefined -- conduitPut (S.put :: S.Putter ClientBoundLogin)


deserializePlay :: Conduit B.ByteString (StateT ProtocolState IO) SBPlay
deserializePlay = undefined -- conduitGet (S.get :: S.Get ServerBoundPlay)


serializePlay :: Conduit CBPlay (StateT ProtocolState IO) B.ByteString
serializePlay = undefined -- conduitPut (S.put :: S.Putter ClientBoundPlay)


handleHandshaking :: Config -> Logger -> Conduit SBHandshaking (StateT ProtocolState IO) CBStatus
handleHandshaking config logger = do
  maybeHandshake <- await
  liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeHandshake
  case maybeHandshake of
    Just (SBHandshake _ _ _ ProtocolStatus) -> do
      maybeHandshake <- await
      liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeHandshake
      lift $ put Status

    Just (SBHandshake _ _ _ ProtocolLogin) -> do
      liftIO $ writeTo logger Debug $ "Switching protocol state to LOGIN"
      lift $ put Login

    Just _ -> liftIO $ writeTo logger Err $ "Expecting ServerBoundHandshake, recieved " ++ show maybeHandshake
    Nothing -> return ()


handleStatus :: Config -> Logger -> Conduit SBStatus (StateT ProtocolState IO) CBStatus
handleStatus config logger = do
  maybeStatus <- await
  case maybeStatus of
    Just SBRequest -> do
      let responsePacket = CBResponse ""
      {-
                            snapshotVersion
                            (toEnum protocolVersion)
                            0
                            (srvMaxPlayers $ config)
                            (srvMotd config)
                            -}
      liftIO $ writeTo logger Debug $ "Sending: " ++ show responsePacket
      yield responsePacket

    Just (SBPing payload) -> do
      let pongPacket = CBPong payload
      liftIO $ writeTo logger Debug $ "Sending: " ++ show pongPacket
      yield pongPacket

    Just _ -> liftIO $ writeTo logger Err $ "Expecting ServerBoundPing, recieved " ++ show maybeStatus
    Nothing -> return ()


handleLogin :: Logger -> Conduit SBLogin (StateT ProtocolState IO) CBLogin
handleLogin logger = do
  maybeLoginStart <- await
  liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeLoginStart
  case maybeLoginStart of
    Just (SBLoginStart username) ->
      do  someUUID <- liftIO nextRandom
          let loginSuccess = CBLoginSuccess someUUID username
          liftIO $ writeTo logger Debug $ "Sending: " ++ show loginSuccess
          yield loginSuccess
          liftIO $ writeTo logger Debug $ "Switching protocol state to PLAY"
          lift $ put Play
    Just _ -> liftIO $ writeTo logger Err $ "Expecting ServerBoundLoginStart, recieved " ++ show maybeLoginStart
    Nothing -> return ()


handlePlay :: Config -> Logger -> Conduit SBPlay (StateT ProtocolState IO) CBPlay
handlePlay config logger = do
  someUUID <- liftIO $ nextRandom
  liftIO $ writeTo logger Debug $ "Starting PLAY session"
{-
  let loginPacket = ClientBoundLogin
                      2566
                      (srvGameMode config)
                      Overworld
                      (srvDifficulty config)
                      (srvMaxPlayers config)
                      (srvWorldType config)
                      True
  liftIO $ writeTo logger Debug $ "Sending: " ++ show loginPacket
  yield loginPacket

  let customPayloadPacket1 = ClientBoundCustomPayload "MC|Brand" "opensandbox"
  liftIO $ writeTo logger Debug $ "Sending: " ++ show customPayloadPacket1
  yield customPayloadPacket1

  let customPayloadPacket2 = ClientBoundCustomPayload "REGISTER" "MC|Brand"
  liftIO $ writeTo logger Debug $ "Sending: " ++ show customPayloadPacket2
  yield customPayloadPacket2

  let difficultyPacket = ClientBoundDifficulty (srvDifficulty config)
  liftIO $ writeTo logger Debug $ "Sending: " ++ show difficultyPacket
  yield difficultyPacket

  let updateTimePacket = ClientBoundUpdateTime 1000 25
  liftIO $ writeTo logger Debug $ "Sending: " ++ show updateTimePacket
  yield updateTimePacket

  let abilitiesPacket = ClientBoundPlayerAbilities 0x02 0 0
  liftIO $ writeTo logger Debug $ "Sending: " ++ show abilitiesPacket
  yield abilitiesPacket

  let heldItemSlotPacket = ClientBoundHeldItemSlot False
  liftIO $ writeTo logger Debug $ "Sending: " ++ show heldItemSlotPacket
  yield heldItemSlotPacket

  let entityStatusPacket = ClientBoundEntityStatus 0 1
  liftIO $ writeTo logger Debug $ "Sending: " ++ show entityStatusPacket
  yield entityStatusPacket

  let statisticsPacket = ClientBoundStatistics (V.fromList [])
  liftIO $ writeTo logger Debug $ "Sending: " ++ show statisticsPacket
  yield statisticsPacket

  let testAction = PlayerListAdd "oldmanmike" V.empty Survival 0 Nothing
  let testPlayer = Player someUUID testAction
  --let playerListItemPacket = ClientBoundPlayerListItem 0 (V.fromList [testPlayer])
  let playerListItemPacket = ClientBoundPlayerListItem 0 V.empty
  liftIO $ writeTo logger Debug $ "Sending: " ++ show playerListItemPacket
  yield playerListItemPacket

  let chunkDataPacket1 = chunkData
  liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket1
  yield chunkDataPacket1

  let chunkDataPacket2 = chunkData
  liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket2
  yield chunkDataPacket2

  let chunkDataPacket3 = chunkData
  liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket3
  yield chunkDataPacket3
-}
