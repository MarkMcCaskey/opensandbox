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

import            Debug.Trace
import            Control.Monad
import            Control.Monad.IO.Class
import            Control.Monad.Trans.Class
import            Control.Monad.Trans.State.Lazy
import            Data.Attoparsec.ByteString
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as Encode
import            Data.Conduit
import            Data.Conduit.Network
import            Data.UUID.V4

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
      writeTo logger Debug "Somebody's handshaking!"
      case firstState of
        Status -> do
          writeTo logger Debug "Beginning Status handling..."
          secondState <- flip execStateT Status
            $ packetSource app
            $$ deserializeStatus
            =$= handleStatus config logger
            =$= serializeStatus
            =$= packetSink app
          writeTo logger Debug "Somebody's pinging!"
          _ <- flip execStateT Status
            $ packetSource app
            $$ deserializeStatus
            =$= handleStatus config logger
            =$= serializeStatus
            =$= packetSink app
          return ()

        Login -> do
          writeTo logger Debug "Beginning Login handling..."
          thirdState <- flip execStateT Login
            $ packetSource app
            $$ deserializeLogin
            =$= handleLogin logger
            =$= serializeLogin
            =$= packetSink app
          if thirdState == Play
            then do
              writeTo logger Debug "Beginning Play handling..."
              void $ flip execStateT Play
                $ packetSource app
                $$ deserializePlay
                =$= handlePlay config logger
                =$= serializePlay
                =$= packetSink app
            else writeTo logger Debug "Somebody failed login"
        _ -> return ()

packetSource  :: AppData -> Source (StateT ProtocolState IO) B.ByteString
packetSource app = transPipe lift $ appSource app


packetSink  :: AppData -> Sink B.ByteString (StateT ProtocolState IO) ()
packetSink app = transPipe lift $ appSink app


deserializeHandshaking :: Conduit B.ByteString (StateT ProtocolState IO) (Either String SBHandshaking)
deserializeHandshaking = do
    maybeBS <- await
    case maybeBS of
      Nothing -> return ()
      Just bs -> do
        case parseOnly (decodeSBHandshaking <* endOfInput) (B.tail bs) of
          Left err -> traceM err >> traceM (show bs) >> yield (Left err) >> leftover bs
          Right handshake -> yield (Right handshake)


deserializeStatus :: Conduit B.ByteString (StateT ProtocolState IO) (Either String SBStatus)
deserializeStatus = do
    maybeBS <- await
    case maybeBS of
      Nothing -> return ()
      Just bs -> do
        case parseOnly (decodeSBStatus <* endOfInput) (B.tail bs) of
          Left err -> traceM err >> traceM (show bs) >> yield (Left err) >> leftover bs
          Right status -> yield (Right status)


serializeStatus :: Conduit CBStatus (StateT ProtocolState IO) B.ByteString
serializeStatus = do
  maybeStatus <- await
  case maybeStatus of
    Nothing -> return ()
    Just status -> do
      let bs = BL.toStrict $ Encode.toLazyByteString (encodeCBStatus status)
      let ln = BL.toStrict $ Encode.toLazyByteString (encodeVarInt . B.length $ bs)
      yield (ln `B.append` bs)


deserializeLogin :: Conduit B.ByteString (StateT ProtocolState IO) (Either String SBLogin)
deserializeLogin = do
  maybeBS <- await
  case maybeBS of
    Nothing -> return ()
    Just bs -> do
      case parseOnly (decodeSBLogin <* endOfInput) (B.tail bs) of
        Left err -> traceM err >> traceM (show bs) >> yield (Left err) >> leftover bs
        Right login -> yield (Right login)


serializeLogin :: Conduit CBLogin (StateT ProtocolState IO) B.ByteString
serializeLogin = do
  maybeLogin <- await
  case maybeLogin of
    Nothing -> return ()
    Just login -> do
      let bs = BL.toStrict $ Encode.toLazyByteString (encodeCBLogin login)
      let ln = BL.toStrict $ Encode.toLazyByteString (encodeVarInt . B.length $ bs)
      yield (ln `B.append` bs)


deserializePlay :: Conduit B.ByteString (StateT ProtocolState IO) SBPlay
deserializePlay = undefined


serializePlay :: Conduit CBPlay (StateT ProtocolState IO) B.ByteString
serializePlay = undefined


handleHandshaking :: Config -> Logger -> Sink (Either String SBHandshaking) (StateT ProtocolState IO) ()
handleHandshaking config logger = do
  maybeHandshake <- await
  liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeHandshake
  case maybeHandshake of
    Nothing -> return ()
    Just eitherHandshake -> do
      case eitherHandshake of
        Left parseErr -> do
          liftIO $ writeTo logger Err $ "Something went wrong: " ++ show parseErr
          return ()
        Right (SBHandshake _ _ _ ProtocolStatus) -> do
          liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeHandshake
          lift $ put Status
          return ()
        Right (SBHandshake _ _ _ ProtocolLogin) -> do
          liftIO $ writeTo logger Debug $ "Switching protocol state to LOGIN"
          lift $ put Login
          return ()
        Right (SBLegacyServerListPing x) -> do
          liftIO $ writeTo logger Debug $ show x
          return ()


handleStatus  :: Config -> Logger -> Conduit (Either String SBStatus) (StateT ProtocolState IO) CBStatus
handleStatus config logger = do
  maybeStatus <- await
  case maybeStatus of
    Nothing -> return ()
    Just eitherStatus -> do
      case eitherStatus of
        Left parseErr -> liftIO $ writeTo logger Err $ parseErr

        Right SBRequest -> do
          let responsePacket = CBResponse
                                snapshotVersion
                                (toEnum protocolVersion)
                                0
                                (srvMaxPlayers $ config)
                                (srvMotd config)

          liftIO $ writeTo logger Debug $ "Sending: " ++ show responsePacket
          yield responsePacket

        Right (SBPing payload) -> do
          let pongPacket = CBPong payload
          liftIO $ writeTo logger Debug $ "Sending: " ++ show pongPacket
          yield pongPacket


handleLogin :: Logger -> Conduit (Either String SBLogin) (StateT ProtocolState IO) CBLogin
handleLogin logger = do
  maybeLoginStart <- await
  liftIO $ writeTo logger Debug $ "Recieving: " ++ show maybeLoginStart
  case maybeLoginStart of
    Nothing -> return ()
    Just eitherLogin -> do
      case eitherLogin of
        Left parseErr -> liftIO $ writeTo logger Err $ parseErr

        Right (SBLoginStart username) -> do
          someUUID <- liftIO nextRandom
          liftIO $ writeTo logger Debug $ "Switching protocol state to PLAY"
          lift $ put Play
          let loginSuccess = CBLoginSuccess someUUID username
          liftIO $ writeTo logger Debug $ "Sending: " ++ show loginSuccess
          yield loginSuccess

        Right (SBEncryptionResponse sharedSecret verifyToken) -> do
          liftIO $ writeTo logger Debug $ "Got an encryption request!"
          return ()


handlePlay  :: Config -> Logger -> Conduit SBPlay (StateT ProtocolState IO) CBPlay
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
