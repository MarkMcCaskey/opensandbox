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

import qualified Codec.Compression.Zlib as Zlib
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.State.Lazy as S (put)
import qualified Data.Attoparsec.ByteString as Decode
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.Network
import Data.Int
import qualified Data.Map.Lazy as ML
import Data.NBT
import Data.Serialize
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Text.Encoding
import Data.UUID.V4
import OpenSandbox.Config
import OpenSandbox.Logger
import OpenSandbox.Protocol
import OpenSandbox.Time
import OpenSandbox.Version
import OpenSandbox.World

logMsg :: Logger -> Lvl -> String -> IO ()
logMsg logger lvl msg = logIO logger "OpenSandbox.Network" lvl (T.pack msg)

runOpenSandboxServer :: Config -> Logger -> Encryption -> MVar Int64 -> World -> IO ()
runOpenSandboxServer config logger encryption worldClock world =
    runTCPServer (serverSettings (srvPort config) "*") $ \app -> do
      firstState <- flip execStateT ProtocolHandshake
        $ packetSource app
        $$ deserializeHandshaking
        =$= handleHandshaking logger
        =$= handleStatus config logger
        =$= serializeStatus
        =$= packetSink app
      liftIO $ logMsg logger LvlDebug $ "Somebody's handshaking!"
      case firstState of
        ProtocolStatus -> do
          liftIO $ logMsg logger LvlDebug $ "Beginning Status handling..."
          _ <- flip execStateT ProtocolStatus
            $ packetSource app
            $$ deserializeStatus
            =$= handleStatus config logger
            =$= serializeStatus
            =$= packetSink app
          liftIO $ logMsg logger LvlDebug $ "Somebody's pinging!"
          _ <- flip execStateT ProtocolStatus
            $ packetSource app
            $$ deserializeStatus
            =$= handleStatus config logger
            =$= serializeStatus
            =$= packetSink app
          return ()
        ProtocolLogin -> do
          liftIO $ logMsg logger LvlDebug $ "Beginning Login handling..."
          thirdState <- flip execStateT ProtocolLogin
            $ packetSource app
            $$ deserializeLogin
            =$= handleLogin config logger encryption
            =$= serializeLogin config
            =$= packetSink app
          if thirdState == ProtocolPlay
            then do
              liftIO $ logMsg logger LvlDebug $ "Beginning Play handling..."
              void $ flip execStateT ProtocolPlay
                $ packetSource app
                $$ deserializePlay config
                =$= handlePlay config logger worldClock world
                =$= serializePlay config
                =$= packetSink app
            else liftIO $ logMsg logger LvlDebug $ "Somebody failed login"
        _ -> return ()

packetSource  :: AppData -> Source (StateT ProtocolState IO) B.ByteString
packetSource app = transPipe liftIO $ appSource app

packetSink  :: AppData -> Sink B.ByteString (StateT ProtocolState IO) ()
packetSink app = transPipe liftIO $ appSink app

{-
compressPlay :: Conduit B.ByteString (StateT ProtocolState IO) B.ByteString
compressPlay = awaitForever $ \play -> yield (BL.toStrict $ Zlib.compress (BL.fromStrict $ play))

decompressPlay :: Conduit B.ByteString (StateT ProtocolState IO) B.ByteString
decompressPlay = awaitForever $ \play -> yield (BL.toStrict $ Zlib.decompress (BL.fromStrict $ play))
-}

deserializeHandshaking :: Conduit B.ByteString (StateT ProtocolState IO) (Either String (SBHandshaking,Maybe SBStatus))
deserializeHandshaking = do
    maybeBS <- await
    case maybeBS of
      Nothing -> return ()
      Just bs -> do
        if B.take 2 bs /= "\254\SOH"
          then do
            case runGet getSBHandshaking' bs of
              Left err -> yield (Left err) >> leftover bs
              Right (handshake,status) -> yield (Right (handshake,status))
          else do
            case Decode.parseOnly (Decode.takeByteString <* Decode.endOfInput) (B.tail bs) of
              Left err -> yield (Left err) >> leftover bs
              Right _ -> yield $ Right (SBLegacyServerListPing,Nothing)
  where
  getSBHandshaking' = do
    ln <- getVarInt
    bs <- getBytes ln
    case decode bs of
      Left err -> fail $ err
      Right handshake -> do
        end <- isEmpty
        if end
          then return (handshake,Nothing)
          else do
            ln' <- getVarInt
            earlyBs <- getBytes ln'
            case decode earlyBs of
              Left _ -> return (handshake,Nothing)
              Right earlyStatus -> return (handshake,Just earlyStatus)

deserializeStatus :: Conduit B.ByteString (StateT ProtocolState IO) (Either String SBStatus)
deserializeStatus = do
    maybeBS <- await
    case maybeBS of
      Nothing -> return ()
      Just bs -> do
        case decode (B.tail bs) of
          Left err -> yield (Left err) >> leftover bs
          Right status -> yield (Right status)

serializeStatus :: Conduit CBStatus (StateT ProtocolState IO) B.ByteString
serializeStatus = do
  maybeStatus <- await
  case maybeStatus of
    Nothing -> return ()
    Just status -> do
      let bs = encode status
      let ln = runPut . putVarInt . B.length $ bs
      yield (ln `B.append` bs)

deserializeLogin :: Conduit B.ByteString (StateT ProtocolState IO) (Either String SBLogin)
deserializeLogin = do
  maybeBS <- await
  case maybeBS of
    Nothing -> return ()
    Just bs -> do
      case decode (B.tail bs) of
        Left err -> yield (Left err) >> leftover bs
        Right login -> yield (Right login)

serializeLogin :: Config -> Conduit CBLogin (StateT ProtocolState IO) B.ByteString
serializeLogin config = do
  maybeLogin <- await
  case maybeLogin of
    Nothing -> return ()
    Just (CBLoginSuccess a b) -> do
      let uncompressedBS = (encodeLazy (CBLoginSuccess a b))
      if srvCompression config
        then do
          let dataLn = runPut . putVarInt . B.length . BL.toStrict $ uncompressedBS
          let compressedBS = BL.toStrict . Zlib.compress $ uncompressedBS
          let packetData = dataLn `B.append` compressedBS
          let packetLn = runPut . putVarInt . B.length $ packetData
          yield (packetLn `B.append` packetData)
        else do
          let bs = encode (CBLoginSuccess a b)
          let ln = runPut . putVarInt . B.length $ bs
          yield (ln `B.append` bs)
    Just login -> do
      let bs = encode login
      let ln = runPut . putVarInt . B.length $ bs
      yield (ln `B.append` bs)

deserializePlay :: Config -> Conduit B.ByteString (StateT ProtocolState IO) (Either String [SBPlay])
deserializePlay config = awaitForever (\bs -> yield $ runGet getPackets bs)
  where
  getPackets = Decode.many1 $ do
    case srvCompression config of
      False -> do
        ln <- getVarInt
        bs' <- getBytes ln
        case decode bs' of
          Left err -> fail err
          Right packet -> return packet
      True -> do
        ln <- getVarInt
        bs' <- getBytes ln
        case runGet getCompressed bs' of
          Left err -> fail err
          Right packet -> return packet
  getCompressed = do
    ln <- getVarInt
    compressedBS <- getLazyByteString (toEnum ln)
    let uncompressedBS = BL.toStrict $ Zlib.decompress compressedBS
    case decode uncompressedBS of
      Left err -> fail err
      Right packet -> return packet

serializePlay :: Config -> Conduit CBPlay (StateT ProtocolState IO) B.ByteString
serializePlay config = awaitForever $ \play -> do
  let uncompressedBS = encodeLazy play
  if srvCompression config
    then do
      let dataLn = runPut . putVarInt . B.length . BL.toStrict $ uncompressedBS
      let compressedBS = BL.toStrict . Zlib.compress $ uncompressedBS
      let packetData = (dataLn `B.append` compressedBS)
      let packetLn = runPut . putVarInt . B.length $ packetData
      yield (packetLn `B.append` packetData)
    else do
      let bs = encode play
      let ln = runPut . putVarInt . B.length $ bs
      yield (ln `B.append` bs)

handleHandshaking :: Logger -> Conduit (Either String (SBHandshaking,Maybe SBStatus)) (StateT ProtocolState IO) (Either String SBStatus)
handleHandshaking logger = do
  maybeHandshake <- await
  liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show maybeHandshake
  case maybeHandshake of
    Nothing -> return ()
    Just eitherHandshake ->
      case eitherHandshake of
        Left parseErr -> do
          liftIO $ logMsg logger LvlError $ "Something went wrong: " ++ show parseErr
          return ()
        Right (SBHandshake _ _ _ ProtocolHandshake,_) -> do
          liftIO $ logMsg logger LvlDebug "Redundant handshake"
          return ()
        Right (SBHandshake _ _ _ ProtocolStatus,status) -> do
          liftIO $ logMsg logger LvlDebug "Switching protocol state to STATUS"
          lift $ S.put ProtocolStatus
          case status of
            Nothing -> return ()
            Just status' -> yield (Right status')
        Right (SBHandshake _ _ _ ProtocolLogin,_) -> do
          liftIO $ logMsg logger LvlDebug "Switching protocol state to LOGIN"
          lift $ S.put ProtocolLogin
          return ()
        Right (SBHandshake _ _ _ ProtocolPlay,_) -> do
          liftIO $ logMsg logger LvlDebug "Rejecting attempt to set protocol state to PLAY"
          return ()
        Right (SBLegacyServerListPing,_)-> do
          liftIO $ logMsg logger LvlDebug "Recieved LegacyServerListPing"
          return ()

handleStatus  :: Config -> Logger -> Conduit (Either String SBStatus) (StateT ProtocolState IO) CBStatus
handleStatus config logger = do
  maybeStatus <- await
  case maybeStatus of
    Nothing -> return ()
    Just eitherStatus ->
      case eitherStatus of
        Left parseErr -> liftIO $ logMsg logger LvlError parseErr
        Right SBRequest -> do
          let responsePacket =
                CBResponse
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

handleLogin :: Config -> Logger -> Encryption -> Conduit (Either String SBLogin) (StateT ProtocolState IO) CBLogin
handleLogin config logger encryption = do
  maybeLoginStart <- await
  liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show maybeLoginStart
  case maybeLoginStart of
    Nothing -> return ()
    Just eitherLogin ->
      case eitherLogin of
        Left parseErr -> liftIO $ logMsg logger LvlError parseErr
        Right (SBLoginStart username) -> do
          someUUID <- liftIO nextRandom
          liftIO $ logMsg logger LvlDebug "Switching protocol state to PLAY"
          lift $ S.put ProtocolPlay
          if srvEncryption config
            then do
              let encryptionRequest = CBEncryptionRequest "" (getCert encryption) (getVerifyToken encryption)
              liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show encryptionRequest
              yield encryptionRequest
              maybeEitherEncryptionResponse <- await
              case maybeEitherEncryptionResponse of
                Nothing -> return ()
                Just eitherEncryptionResponse ->
                  case eitherEncryptionResponse of
                    Left err -> liftIO $ logMsg logger LvlError err

                    Right (SBEncryptionResponse _ _) -> do
                      liftIO $ logMsg logger LvlDebug "Got an encryption request!"
                      when (srvCompression config) $ do
                        let setCompression = CBSetCompression 0
                        liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show setCompression
                        yield setCompression
                      let loginSuccess = CBLoginSuccess someUUID username
                      liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show loginSuccess
                      yield loginSuccess

                    Right (SBLoginStart _) -> do
                      liftIO $ logMsg logger LvlError "Redundant SBLoginStart!"
                      return ()
            else do
              when (srvCompression config) $ do
                let setCompression = CBSetCompression 0
                liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show setCompression
                yield setCompression
              let loginSuccess = CBLoginSuccess someUUID username
              liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show loginSuccess
              yield loginSuccess

        Right (SBEncryptionResponse _ _) -> do
          liftIO $ logMsg logger LvlError "Got an encryption request out of order!"
          return ()

handlePlay  :: Config -> Logger -> MVar Int64 -> World -> Conduit (Either String [SBPlay]) (StateT ProtocolState IO) CBPlay
handlePlay config logger worldClock world = do
  someUUID <- liftIO nextRandom
  liftIO $ logMsg logger LvlDebug "Starting PLAY session"
  let loginPacket =
        CBJoinGame
          2566
          (srvGameMode config)
          (srvDimension config)
          (srvDifficulty config)
          (srvMaxPlayers config)
          (T.pack . show $ srvWorldType config)
          True

  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show loginPacket
  yield loginPacket

  let customPayloadPacket1 = CBPluginMessage "MC|Brand" (encodeUtf8 "opensandbox")
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show customPayloadPacket1
  yield customPayloadPacket1

  let customPayloadPacket2 = CBPluginMessage "REGISTER" (encodeUtf8 "MC|Brand")
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show customPayloadPacket2
  yield customPayloadPacket2

  let difficultyPacket = CBServerDifficulty (srvDifficulty config)
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show difficultyPacket
  yield difficultyPacket

  let spawnPositionPacket = CBSpawnPosition 0
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show spawnPositionPacket
  yield spawnPositionPacket

  let playerAbilitiesPacket = CBPlayerAbilities 0 1028443341 0
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show playerAbilitiesPacket
  yield playerAbilitiesPacket

  let heldItemChangePacket = CBHeldItemChange 0
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show heldItemChangePacket
  yield heldItemChangePacket

  let entityStatusPacket = CBEntityStatus 32 AnimalInLove
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show entityStatusPacket
  yield entityStatusPacket

  let statisticsPacket = CBStatistics []
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show statisticsPacket
  yield statisticsPacket

  let testAction = PlayerListAdd someUUID "oldmanmike" [] Survival 0 Nothing
  let playerListItemPacket = CBPlayerListItem (PlayerListAdds [testAction])
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show playerListItemPacket
  yield playerListItemPacket

  let playerPositionAndLookPacket = CBPlayerPositionAndLook 0 4 0 0 0 0 777
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show playerPositionAndLookPacket
  yield playerPositionAndLookPacket

  let worldBorderAction = Initialize 0 0 4723321873536909312 4723321873536909312 0 29999984 5 15
  let worldBorderPacket = CBWorldBorder worldBorderAction
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show worldBorderPacket
  yield worldBorderPacket

  worldAge <- liftIO $ getWorldAge worldClock
  worldTime <- liftIO $ getWorldTime worldClock
  let updateTimePacket = CBTimeUpdate worldAge worldTime
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show updateTimePacket
  yield updateTimePacket

  let windowItemsPacket = CBWindowItems 0 (V.replicate 46 (mkSlot (-1) 1 1 (NBT "" (ByteTag 0))))
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show windowItemsPacket
  yield windowItemsPacket

  let setSlotPacket = CBSetSlot (-1) (-1) (mkSlot (-1) 1 1 (NBT "" (ByteTag 0)))
  liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show setSlotPacket
  yield setSlotPacket

  mapM_ (yield . CBChunkData) $ ML.elems world

  --let entityMetadata = CBEntityMetadata 0 (V.singleton (Entry 13 MetadataByte ))
  --liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show entityMetadata
  --yield entityMetadata

  awaitForever $ \eitherPackets ->
    case eitherPackets of
      Left err -> liftIO $ logMsg logger LvlError err
      Right packets -> do
        liftIO $ threadDelay 10000
        liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show packets
        mapM_ handle packets
        age <- liftIO $ getWorldAge worldClock
        t <- liftIO $ getWorldTime worldClock
        when (mod t 20 == 0) $ do
          liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show (CBTimeUpdate age t)
          yield (CBTimeUpdate age t)
        when (mod t 40 == 0) $ do
          liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show (CBKeepAlive 5346)
          yield (CBKeepAlive 5346)
  where
    handle packet =
      case packet of
        SBTeleportConfirm {} -> return ()
        SBTabComplete {} -> return ()
        SBChatMessage {} -> return ()
        SBClientStatus {} -> return ()
        SBClientSettings {} -> return ()
        SBConfirmTransaction {} -> return ()
        SBEnchantItem {} -> return ()
        SBClickWindow {} -> return ()
        SBCloseWindow {} -> return ()
        SBPluginMessage {} -> return ()
        SBUseEntity {} -> return ()
        SBKeepAlive {} -> return ()
        SBPlayerPosition {} -> return ()
        SBPlayerPositionAndLook {} -> return ()
        SBPlayerLook {} -> return ()
        SBPlayer {} -> return ()
        SBVehicleMove {} -> return ()
        SBSteerBoat {} -> return ()
        SBPlayerAbilities {} -> return ()
        SBPlayerDigging {} -> return ()
        SBEntityAction {} -> return ()
        SBSteerVehicle {} -> return ()
        SBResourcePackStatus {} -> return ()
        SBHeldItemChange {} -> return ()
        SBCreativeInventoryAction {} -> return ()
        SBUpdateSign {} -> return ()
        SBAnimation {} -> return ()
        SBSpectate {} -> return ()
        SBPlayerBlockPlacement {} -> return ()
        SBUseItem {} -> return ()
