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

import            Debug.Trace
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
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID.V4
import qualified  Data.Vector as V

import            OpenSandbox.Config
import            OpenSandbox.Logger
import            OpenSandbox.Protocol
import            OpenSandbox.Types
import            OpenSandbox.Version

runOpenSandboxServer :: Config -> Logger -> Encryption -> IO ()
runOpenSandboxServer config logger encryption =
    runTCPServer (serverSettings (srvPort config) "*") $ \app -> do
      firstState <- flip execStateT Handshake
        $ packetSource app
        $$ deserializeHandshaking
        =$= handleHandshaking config logger
        =$= handleStatus config logger
        =$= serializeStatus
        =$= packetSink app
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

deserializePlay :: Conduit B.ByteString (StateT ProtocolState IO) (Either String SBPlay)
deserializePlay = do
  maybeBS <- await
  case maybeBS of
    Nothing -> return ()
    Just bs -> do
      case Decode.parseOnly (decodeSBPlay <* Decode.endOfInput) (B.tail bs) of
        Left err -> yield (Left err) >> leftover bs
        Right play -> yield (Right play)


serializePlay :: Conduit CBPlay (StateT ProtocolState IO) B.ByteString
serializePlay = awaitForever (\play -> do
      let bs = BL.toStrict . Encode.toLazyByteString . encodeCBPlay $ play
      let ln = BL.toStrict . Encode.toLazyByteString . encodeVarInt . B.length $ bs
      yield (ln `B.append` bs)
  )


handleHandshaking :: Config -> Logger -> Conduit (Either String (SBHandshaking,Maybe SBStatus)) (StateT ProtocolState IO) (Either String SBStatus)
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
        Right ((SBHandshake _ _ _ ProtocolStatus),status) -> do
          liftIO $ writeTo logger Debug $ "Switching protocol state to STATUS"
          lift $ put Status
          case status of
            Nothing -> return ()
            Just status' -> yield (Right status')
        Right ((SBHandshake _ _ _ ProtocolLogin),_) -> do
          liftIO $ writeTo logger Debug $ "Switching protocol state to LOGIN"
          lift $ put Login
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


handlePlay  :: Config -> Logger -> Conduit (Either String SBPlay) (StateT ProtocolState IO) CBPlay
handlePlay config logger = do
  someUUID <- liftIO $ nextRandom
  liftIO $ writeTo logger Debug $ "Starting PLAY session"
  let loginPacket = CBJoinGame
                      2566
                      (toEnum . fromEnum $ srvGameMode config)
                      (toEnum . fromEnum $ srvDimension config)
                      (toEnum . fromEnum $ srvDifficulty config)
                      (srvMaxPlayers config)
                      (T.pack . show $ srvWorldType config)
                      True

  liftIO $ writeTo logger Debug $ "Sending: " ++ show loginPacket
  yield loginPacket

  let customPayloadPacket1 = CBPluginMessage "MC|Brand" (encodeUtf8 "opensandbox")
  liftIO $ writeTo logger Debug $ "Sending: " ++ show customPayloadPacket1
  yield customPayloadPacket1

  let customPayloadPacket2 = CBPluginMessage "REGISTER" (encodeUtf8 "MC|Brand")
  liftIO $ writeTo logger Debug $ "Sending: " ++ show customPayloadPacket2
  yield customPayloadPacket2

  let difficultyPacket = CBServerDifficulty (toEnum . fromEnum $ srvDifficulty config)
  liftIO $ writeTo logger Debug $ "Sending: " ++ show difficultyPacket
  yield difficultyPacket

  let spawnPositionPacket = CBSpawnPosition 0
  liftIO $ writeTo logger Debug $ "Sending: " ++ show spawnPositionPacket
  yield spawnPositionPacket

  let playerAbilitiesPacket = CBPlayerAbilities 0 1028443341 1036831949
  liftIO $ writeTo logger Debug $ "Sending: " ++ show playerAbilitiesPacket
  yield playerAbilitiesPacket

  let heldItemChangePacket = CBHeldItemChange 0
  liftIO $ writeTo logger Debug $ "Sending: " ++ show heldItemChangePacket
  yield heldItemChangePacket

  let entityStatusPacket = CBEntityStatus 32 AnimalInLove
  liftIO $ writeTo logger Debug $ "Sending: " ++ show entityStatusPacket
  yield entityStatusPacket

  let statisticsPacket = CBStatistics []
  liftIO $ writeTo logger Debug $ "Sending: " ++ show statisticsPacket
  yield statisticsPacket

  let testAction = PlayerListAdd someUUID "oldmanmike" [] SurvivalField 0 Nothing
  let playerListItemPacket = CBPlayerListItem (PlayerListAdds [testAction])
  liftIO $ writeTo logger Debug $ "Sending: " ++ show playerListItemPacket
  yield playerListItemPacket

  let playerPositionAndLookPacket = CBPlayerPositionAndLook 0 100 0 0 0 0 0
  liftIO $ writeTo logger Debug $ "Sending: " ++ show playerPositionAndLookPacket
  yield playerPositionAndLookPacket

  let worldBorderAction = Initialize 0 0 100 100 0 29999984 5 15
  let worldBorderPacket = CBWorldBorder worldBorderAction
  liftIO $ writeTo logger Debug $ "Sending: " ++ show worldBorderPacket
  yield worldBorderPacket

  let updateTimePacket = CBTimeUpdate 1000 25
  liftIO $ writeTo logger Debug $ "Sending: " ++ show updateTimePacket
  yield updateTimePacket

  let windowItemsPacket = CBWindowItems 0 (V.replicate 46 (mkSlot (-1) 1 1 (TagByte "" 0)))
  liftIO $ writeTo logger Debug $ "Sending: " ++ show windowItemsPacket
  yield windowItemsPacket

  let setSlotPacket = CBSetSlot (-1) (-1) (mkSlot (-1) 1 1 (TagByte "" 0))
  liftIO $ writeTo logger Debug $ "Sending: " ++ show setSlotPacket
  yield setSlotPacket

  let bedrockLayer = V.replicate 16 $ V.foldl' (xor) 0 $ V.zipWith setBit (V.replicate 16 (0 :: Int64)) [0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60]
  let dirtLayer = V.replicate 16 $ V.foldl' (xor) 0 $ V.zipWith setBit (V.replicate 16 (0 :: Int64)) [1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61]
  let grassLayer = V.replicate 16 $ V.foldl' (xor) 0 $ V.zipWith setBit (V.replicate 32 (0 :: Int64)) [0,1,4,5,8,9,12,13,16,17,20,21,24,25,28,29,32,33,36,37,40,41,44,45,48,49,52,53,56,57,60,61]
  let airLayer = V.replicate 16 (0 :: Int64)
  let flatWorldBase = V.concat [bedrockLayer,dirtLayer,dirtLayer,grassLayer,(V.concat $ replicate 12 airLayer)]
  let chunkSection1 = OverWorldChunkSection 4 [0,112,48,32] flatWorldBase (B.replicate 2048 0) (B.replicate 2048 0)
  let chunkSections1 = OverWorldChunkSections [chunkSection1]
  let chunkDataPacket1 = CBChunkData 0 0 1 chunkSections1 (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket1
  yield chunkDataPacket1

  let airChunk = V.concat $ replicate 16 airLayer
  let chunkSectionX = OverWorldChunkSection 4 [0,112,48,32] airChunk (B.replicate 2048 0) (B.replicate 2048 0)
  let chunkSectionsX = OverWorldChunkSections [chunkSectionX]

  let chunkDataPacket2 = CBChunkData 0 0 2 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket2
  yield chunkDataPacket2

  let chunkDataPacket3 = CBChunkData 0 0 4 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket3
  yield chunkDataPacket3

  let chunkDataPacket4 = CBChunkData 0 0 8 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket4
  yield chunkDataPacket4

  let chunkDataPacket5 = CBChunkData 0 0 16 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket5
  yield chunkDataPacket5

  let chunkDataPacket6 = CBChunkData 0 0 32 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket6
  yield chunkDataPacket6

  let chunkDataPacket7 = CBChunkData 0 0 64 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket7
  yield chunkDataPacket7

  let chunkDataPacket8 = CBChunkData 0 0 128 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket8
  yield chunkDataPacket8

  let chunkDataPacket9 = CBChunkData 0 0 256 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket9
  yield chunkDataPacket9

  let chunkDataPacket10 = CBChunkData 0 0 512 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket10
  yield chunkDataPacket10

  let chunkDataPacket11 = CBChunkData 0 0 1024 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket11
  yield chunkDataPacket11

  let chunkDataPacket12 = CBChunkData 0 0 2048 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket12
  yield chunkDataPacket12

  let chunkDataPacket13 = CBChunkData 0 0 4096 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket13
  yield chunkDataPacket13

  let chunkDataPacket14 = CBChunkData 0 0 8192 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket14
  yield chunkDataPacket14

  let chunkDataPacket15 = CBChunkData 0 0 16384 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket15
  yield chunkDataPacket15

  let chunkDataPacket16 = CBChunkData 0 0 32768 chunkSectionsX (Just $ B.replicate 256 1) V.empty
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show chunkDataPacket16
  yield chunkDataPacket16

  let keepAlivePacket = CBKeepAlive 100
  --liftIO $ writeTo logger Debug $ "Sending: " ++ show keepAlivePacket
  yield keepAlivePacket

{-
  let disconnectPacket = CBPlayDisconnect "Because I said so..."
  liftIO $ writeTo logger Debug $ "Sending: " ++ show disconnectPacket
  yield disconnectPacket
-}
