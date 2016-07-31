{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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

import            Control.Concurrent (threadDelay)
import            Control.Monad
import            Control.Monad.IO.Class
import            Control.Monad.Trans.Class
import            Control.Monad.Trans.State.Lazy
import qualified  Data.Attoparsec.ByteString as Decode
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as Encode
import            Data.Conduit
import            Data.Conduit.Network
import            Data.Int
import            Data.List
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID.V4
import qualified  Data.Vector as V

import            OpenSandbox.Config
import            OpenSandbox.Logger
import            OpenSandbox.Data.Protocol
import            OpenSandbox.Version
import            OpenSandbox.World

logMsg :: MonadIO m => Logger -> Lvl -> String -> m ()
logMsg logger lvl msg = logIO logger "OpenSandbox.Network" lvl (T.pack msg)

runOpenSandboxServer :: Config -> Logger -> Encryption -> IO ()
runOpenSandboxServer config logger encryption =
    runTCPServer (serverSettings (srvPort config) "*") $ \app -> do
      firstState <- flip execStateT ProtocolHandshake
        $ packetSource app
        $$ deserializeHandshaking
        =$= handleHandshaking config logger
        =$= handleStatus config logger
        =$= serializeStatus
        =$= packetSink app
      liftIO $ logMsg logger LvlDebug $ "Somebody's handshaking!"
      case firstState of
        ProtocolStatus -> do
          logMsg logger LvlDebug $ "Beginning Status handling..."
          secondState <- flip execStateT ProtocolStatus
            $ packetSource app
            $$ deserializeStatus
            =$= handleStatus config logger
            =$= serializeStatus
            =$= packetSink app
          logMsg logger LvlDebug $ "Somebody's pinging!"
          _ <- flip execStateT ProtocolStatus
            $ packetSource app
            $$ deserializeStatus
            =$= handleStatus config logger
            =$= serializeStatus
            =$= packetSink app
          return ()
        ProtocolLogin -> do
          logMsg logger LvlDebug $ "Beginning Login handling..."
          thirdState <- flip execStateT ProtocolLogin
            $ packetSource app
            $$ deserializeLogin
            =$= handleLogin logger
            =$= serializeLogin
            =$= packetSink app
          if thirdState == ProtocolPlay
            then do
              logMsg logger LvlDebug $ "Beginning Play handling..."
              void $ flip execStateT ProtocolPlay
                $ packetSource app
                $$ deserializePlay
                =$= handlePlay config logger
                =$= serializePlay
                =$= packetSink app
            else logMsg logger LvlDebug $ "Somebody failed login"
        _ -> return ()


packetSource  :: AppData -> Source (StateT ProtocolState IO) B.ByteString
packetSource app = transPipe lift $ appSource app


packetSink  :: AppData -> Sink B.ByteString (StateT ProtocolState IO) ()
packetSink app = transPipe lift $ appSink app


deserializeHandshaking :: Conduit B.ByteString (StateT ProtocolState IO) (Either String (SBHandshaking,Maybe SBStatus))
deserializeHandshaking = do
    maybeBS <- await
    case maybeBS of
      Nothing -> return ()
      Just bs -> do
        if B.take 2 bs /= "\254\SOH"
          then do
            case Decode.parseOnly decodeSBHandshaking' bs of
              Left err -> yield (Left err) >> leftover bs
              Right (handshake,status) -> yield (Right (handshake,status))
          else do
            case Decode.parseOnly (Decode.takeByteString <* Decode.endOfInput) (B.tail bs) of
              Left err -> yield (Left err) >> leftover bs
              Right handshake -> yield $ Right (SBLegacyServerListPing,Nothing)
  where
  decodeSBHandshaking' = do
    ln <- decodeVarInt
    bs <- Decode.take ln
    case Decode.parseOnly decodeSBHandshaking bs of
      Left err -> fail $ err
      Right handshake -> do
        end <- Decode.atEnd
        if end
          then return (handshake,Nothing)
          else do
            ln' <- decodeVarInt
            earlyBs <- Decode.take ln'
            case Decode.parseOnly decodeSBStatus earlyBs of
              Left err -> return (handshake,Nothing)
              Right earlyStatus -> return (handshake,Just earlyStatus)


deserializeStatus :: Conduit B.ByteString (StateT ProtocolState IO) (Either String SBStatus)
deserializeStatus = do
    maybeBS <- await
    case maybeBS of
      Nothing -> return ()
      Just bs -> do
        case Decode.parseOnly (decodeSBStatus <* Decode.endOfInput) (B.tail bs) of
          Left err -> yield (Left err) >> leftover bs
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
      case Decode.parseOnly (decodeSBLogin <* Decode.endOfInput) (B.tail bs) of
        Left err -> yield (Left err) >> leftover bs
        Right login -> yield (Right login)


serializeLogin :: Conduit CBLogin (StateT ProtocolState IO) B.ByteString
serializeLogin = do
  maybeLogin <- await
  case maybeLogin of
    Nothing -> return ()
    Just login -> do
      let bs = BL.toStrict . Encode.toLazyByteString . encodeCBLogin $ login
      let ln = BL.toStrict . Encode.toLazyByteString . encodeVarInt . B.length $ bs
      yield (ln `B.append` bs)

deserializePlay :: Conduit B.ByteString (StateT ProtocolState IO) (Either String [SBPlay])
deserializePlay = awaitForever (\bs -> yield $ Decode.parseOnly decodePackets bs)
  where
  decodePackets = Decode.many1 $ do
    ln <- decodeVarInt
    bs' <- Decode.take ln
    case Decode.parseOnly decodeSBPlay bs' of
      Left err -> fail err
      Right packet -> return packet


serializePlay :: Conduit CBPlay (StateT ProtocolState IO) B.ByteString
serializePlay = awaitForever (\play -> do
      let bs = BL.toStrict . Encode.toLazyByteString . encodeCBPlay $ play
      let ln = BL.toStrict . Encode.toLazyByteString . encodeVarInt . B.length $ bs
      yield (ln `B.append` bs)
  )


handleHandshaking :: Config -> Logger -> Conduit (Either String (SBHandshaking,Maybe SBStatus)) (StateT ProtocolState IO) (Either String SBStatus)
handleHandshaking config logger = do
  maybeHandshake <- await
  liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show maybeHandshake
  case maybeHandshake of
    Nothing -> return ()
    Just eitherHandshake -> do
      case eitherHandshake of
        Left parseErr -> do
          liftIO $ logMsg logger LvlError $ "Something went wrong: " ++ show parseErr
          return ()
        Right ((SBHandshake _ _ _ ProtocolStatus),status) -> do
          liftIO $ logMsg logger LvlDebug "Switching protocol state to STATUS"
          lift $ put ProtocolStatus
          case status of
            Nothing -> return ()
            Just status' -> yield (Right status')
        Right ((SBHandshake _ _ _ ProtocolLogin),_) -> do
          liftIO $ logMsg logger LvlDebug "Switching protocol state to LOGIN"
          lift $ put ProtocolLogin
          return ()
        Right (SBLegacyServerListPing,_)-> do
          return ()


handleStatus  :: Config -> Logger -> Conduit (Either String SBStatus) (StateT ProtocolState IO) CBStatus
handleStatus config logger = do
  maybeStatus <- await
  case maybeStatus of
    Nothing -> return ()
    Just eitherStatus -> do
      case eitherStatus of
        Left parseErr -> liftIO $ logMsg logger LvlError $ parseErr

        Right SBRequest -> do
          let responsePacket = CBResponse
                                snapshotVersion
                                (toEnum protocolVersion)
                                0
                                (toEnum . fromEnum . srvMaxPlayers $ config)
                                (srvMotd config)

          liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show responsePacket
          yield responsePacket

        Right (SBPing payload) -> do
          let pongPacket = CBPong payload
          liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show pongPacket
          yield pongPacket


handleLogin :: Logger -> Conduit (Either String SBLogin) (StateT ProtocolState IO) CBLogin
handleLogin logger = do
  maybeLoginStart <- await
  liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show maybeLoginStart
  case maybeLoginStart of
    Nothing -> return ()
    Just eitherLogin -> do
      case eitherLogin of
        Left parseErr -> liftIO $ logMsg logger LvlError $ parseErr

        Right (SBLoginStart username) -> do
          someUUID <- liftIO nextRandom
          liftIO $ logMsg logger LvlDebug $ "Switching protocol state to PLAY"
          lift $ put ProtocolPlay
          let loginSuccess = CBLoginSuccess someUUID username
          liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show loginSuccess
          yield loginSuccess

        Right (SBEncryptionResponse sharedSecret verifyToken) -> do
          liftIO $ logMsg logger LvlDebug $ "Got an encryption request!"
          return ()


handlePlay  :: Config -> Logger -> Conduit (Either String [SBPlay]) (StateT ProtocolState IO) CBPlay
handlePlay config logger = do
  someUUID <- liftIO $ nextRandom
  --liftIO $ writeTo logger Debug $ "Starting PLAY session"
  let loginPacket = CBJoinGame
                      2566
                      (toEnum . fromEnum $ srvGameMode config)
                      (toEnum . fromEnum $ srvDimension config)
                      (toEnum . fromEnum $ srvDifficulty config)
                      (toEnum . fromEnum $ srvMaxPlayers config)
                      (T.pack . show $ srvWorldType config)
                      True

  --liftIO $ writeTo logger Debug $ "Sending: " ++ show loginPacket
  yield loginPacket

  let customPayloadPacket1 = CBPluginMessage "MC|Brand" (encodeUtf8 "opensandbox")
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show customPayloadPacket1
  yield customPayloadPacket1

  let customPayloadPacket2 = CBPluginMessage "REGISTER" (encodeUtf8 "MC|Brand")
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show customPayloadPacket2
  yield customPayloadPacket2

  let difficultyPacket = CBServerDifficulty (toEnum . fromEnum $ srvDifficulty config)
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show difficultyPacket
  yield difficultyPacket

  let spawnPositionPacket = CBSpawnPosition 0
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show spawnPositionPacket
  yield spawnPositionPacket

  let playerAbilitiesPacket = CBPlayerAbilities 0 1028443341 1036831949
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show playerAbilitiesPacket
  yield playerAbilitiesPacket

  let heldItemChangePacket = CBHeldItemChange 0
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show heldItemChangePacket
  yield heldItemChangePacket

  let entityStatusPacket = CBEntityStatus 32 AnimalInLove
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show entityStatusPacket
  yield entityStatusPacket

  let statisticsPacket = CBStatistics []
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show statisticsPacket
  yield statisticsPacket

  let testAction = PlayerListAdd someUUID "oldmanmike" [] Survival 0 Nothing
  let playerListItemPacket = CBPlayerListItem (PlayerListAdds [testAction])
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show playerListItemPacket
  yield playerListItemPacket

  let playerPositionAndLookPacket = CBPlayerPositionAndLook 0 4 0 0 0 0 777
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show playerPositionAndLookPacket
  yield playerPositionAndLookPacket

  let worldBorderAction = Initialize 0 0 4723321873536909312 4723321873536909312 0 29999984 5 15
  let worldBorderPacket = CBWorldBorder worldBorderAction
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show worldBorderPacket
  yield worldBorderPacket

  let updateTimePacket = CBTimeUpdate 1000 25
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show updateTimePacket
  yield updateTimePacket

  let windowItemsPacket = CBWindowItems 0 (V.replicate 46 (mkSlot (-1) 1 1 (TagByte "" 0)))
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show windowItemsPacket
  yield windowItemsPacket

  let setSlotPacket = CBSetSlot (-1) (-1) (mkSlot (-1) 1 1 (TagByte "" 0))
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show setSlotPacket
  yield setSlotPacket

  let keepAlivePacket = CBKeepAlive 100
  mapM_ yield $ genFlatWorld (toEnum . fromEnum . srvViewDistance $ config)
  awaitForever $ \packets -> do
    liftIO $ threadDelay 10000
    liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show packets
