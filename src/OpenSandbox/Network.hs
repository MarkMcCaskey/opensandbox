{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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

import Crypto.PubKey.RSA.PKCS15 (decryptSafer)
import qualified Codec.Compression.Zlib as Zlib
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT,execStateT)
import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.Attoparsec.ByteString as Decode
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Int
import Data.List
import Data.Maybe
import Data.NBT
import Data.Serialize
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Text.Encoding
import Data.UUID.V4
import OpenSandbox.Config
import OpenSandbox.Event
import OpenSandbox.Logger
import OpenSandbox.Protocol
import OpenSandbox.Server
import OpenSandbox.Time
import OpenSandbox.User
import OpenSandbox.Version
import OpenSandbox.World

logMsg :: Logger -> Lvl -> String -> IO ()
logMsg logger lvl msg = logIO logger "OpenSandbox.Network" lvl (T.pack msg)

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
                =$= handlePlay config logger worldClock world journal
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

deserializeHandshaking :: Conduit B.ByteString (StateT ProtocolState IO) (SBHandshaking,Maybe SBStatus)
deserializeHandshaking = do
    maybeBS <- await
    case maybeBS of
      Nothing -> return ()
      Just bs ->
        if B.take 2 bs /= "\254\SOH"
          then
            case runGet getSBHandshaking' bs of
              Left _ -> leftover bs
              Right (handshake,status) -> yield (handshake,status)
          else
            case Decode.parseOnly (Decode.takeByteString <* Decode.endOfInput) (B.tail bs) of
              Left _ -> leftover bs
              Right _ -> yield (SBLegacyServerListPing,Nothing)
  where
  getSBHandshaking' = do
    ln <- getVarInt
    bs <- getBytes ln
    case decode bs of
      Left err -> fail err
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

breakupPackets :: (MonadIO m, MonadThrow m) => Conduit B.ByteString m B.ByteString
breakupPackets = awaitForever $ \stream -> mapM_ yield (breakup stream)
  where
    breakup :: B.ByteString -> [B.ByteString]
    breakup "" = []
    breakup bs =
      case runGetPartial getVarInt bs of
        Fail _ _ -> error "Error: Failed to breakup packets!"
        Partial _ -> error "Error: Partial breakup?"
        Done ln leftover -> (B.take ln leftover):breakup (B.drop ln leftover)

fmtPacket :: (MonadIO m) => Conduit B.ByteString m B.ByteString
fmtPacket = awaitForever $ \payload ->
  yield ((runPut . putVarInt . B.length $ payload) `B.append` payload)

deserializePacket :: (Serialize a, MonadThrow m) => Conduit B.ByteString m a
deserializePacket = conduitGet2 get

serializePacket :: (Serialize a, MonadIO m) => Conduit a m B.ByteString
serializePacket = conduitPut put

compressPacket :: Conduit B.ByteString (StateT Session IO) B.ByteString
compressPacket = awaitForever $ \packet -> do
  s <- lift State.get
  if (sessionCompressionIsActive s) && (sessionCompressionIsEnabled s)
    then do
      let dataLn = runPut . putVarInt . B.length $ packet
      let compressedBS = BL.toStrict . Zlib.compress . BL.fromStrict $ packet
      let packetData = dataLn `B.append` compressedBS
      yield packetData
    else yield packet

decompressPacket :: Conduit B.ByteString (StateT Session IO) B.ByteString
decompressPacket = awaitForever $ \compressedBS -> do
  s <- lift State.get
  if (sessionCompressionIsActive s) && (sessionCompressionIsEnabled s)
    then case runGet getCompressed compressedBS of
           Left err -> error err
           Right decompressedBS -> yield decompressedBS
    else yield compressedBS
 where
  getCompressed = do
    _ <- getVarInt
    r <- remaining
    compressedBS <- getLazyByteString (toEnum r)
    return $ BL.toStrict $ Zlib.decompressWith Zlib.defaultDecompressParams compressedBS

encryptPacket :: Conduit B.ByteString (StateT Session IO) B.ByteString
encryptPacket = awaitForever $ \packet -> do
  s <- lift State.get
  if (sessionEncryptionIsActive s) && (sessionEncryptionIsEnabled s)
    then
      case sessionSharedSecret s of
        Nothing -> error "Could not get shared secret from session state!"
        Just sharedSecret -> yield (B.concat . fmap (encrypt sharedSecret) . breakup $ packet)
    else yield packet
  where
    breakup :: B.ByteString -> [B.ByteString]
    breakup bs = if B.length bs > 16
                 then (B.take 16 bs):breakup (B.drop 16 bs)
                 else [bs]

decryptPacket :: Conduit B.ByteString (StateT Session IO) B.ByteString
decryptPacket = awaitForever $ \packet -> do
  s <- lift State.get
  if (sessionEncryptionIsActive s) && (sessionEncryptionIsEnabled s)
    then
      case sessionSharedSecret s of
        Nothing -> error "Could not get shared secret from session state!"
        Just sharedSecret -> yield (B.concat . fmap (decrypt sharedSecret) . breakup $ packet)
    else yield packet
  where
    breakup :: B.ByteString -> [B.ByteString]
    breakup bs = if B.length bs > 16
                 then (B.take 16 bs):breakup (B.drop 16 bs)
                 else [bs]

serializeLogin :: Conduit CBLogin (StateT Session IO) B.ByteString
serializeLogin = do
  maybePacket <- await
  session <- lift $ State.get
  case maybePacket of
    Nothing -> return ()
    Just (CBLoginDisconnect _) -> return ()
    Just (CBEncryptionRequest serverID pubKey token) -> do
      handlePlain (CBEncryptionRequest serverID pubKey token)
      maybePacket <- await
      case maybePacket of
        Nothing -> return ()
        Just CBLoginDisconnect {} -> return ()
        Just CBEncryptionRequest {} -> return ()
        Just (CBLoginSuccess a b) -> do
          s <- lift State.get
          if (sessionEncryptionIsEnabled s) && (isJust . sessionSharedSecret $ s)
            then do
              lift $ State.modify $ \s ->
                s { sessionProtoState = ProtocolPlay
                  , sessionCompressionIsActive = False
                  , sessionEncryptionIsActive = True}
              handleWithEncryption (fromJust . sessionSharedSecret $ s) (CBLoginSuccess a b)
            else do
              lift $ State.modify $ \s ->
                s { sessionProtoState = ProtocolPlay
                  , sessionCompressionIsActive = False
                  , sessionEncryptionIsActive = False}
              handlePlain (CBLoginSuccess a b)
        Just (CBSetCompression threshold) -> do
          s <- lift State.get
          when ((sessionEncryptionIsEnabled s) && (isJust . sessionSharedSecret $ s)) $ do
              lift $ State.modify $ \s ->
                s {sessionEncryptionIsActive = True}
              handleWithEncryption (fromJust . sessionSharedSecret $ s) (CBSetCompression threshold)
              lift $ State.modify $ \s ->
                s {sessionCompressionIsActive = True
                  , sessionEncryptionIsActive = True}
              maybePacket2 <- await
              case maybePacket2 of
                Nothing -> return ()
                Just (CBLoginSuccess a b) -> do
                  lift $ State.modify $ \s ->
                    s { sessionProtoState = ProtocolPlay
                      , sessionCompressionIsActive = True
                      , sessionEncryptionIsActive = True}
                  handleWithBoth (fromJust . sessionSharedSecret $ s) (CBLoginSuccess a b)
                Just _ -> return ()
    Just (CBLoginSuccess a b) -> do
      lift $ State.modify $ \s ->
        s {sessionProtoState = ProtocolPlay}
      if (sessionCompressionIsActive session) && (sessionCompressionIsEnabled session)
        then
          handleWithCompression (CBLoginSuccess a b)
        else
          handlePlain (CBLoginSuccess a b)
    Just (CBSetCompression threshold) -> do
      handlePlain (CBSetCompression threshold)
      maybeLoginSuccess <- await
      case maybeLoginSuccess of
        Nothing -> return ()
        Just loginSuccess -> do
          lift $ State.modify $ \s ->
            s { sessionProtoState = ProtocolPlay
              , sessionCompressionIsActive = True}
          handleWithCompression loginSuccess

handleWithCompression :: (Serialize a, Monad m) => a -> ConduitM a B.ByteString m ()
handleWithCompression bs = do
  let uncompressedBS = encodeLazy bs
  let dataLn = runPut . putVarInt . B.length . BL.toStrict $ uncompressedBS
  let compressedBS = BL.toStrict . Zlib.compress $ uncompressedBS
  let packetData = dataLn `B.append` compressedBS
  let packetLn = runPut . putVarInt . B.length $ packetData
  yield (packetLn `B.append` packetData)

handlePlain :: (Serialize a, Monad m) => a -> ConduitM a B.ByteString m ()
handlePlain packet = do
  let bs = encode packet
  let ln = runPut . putVarInt . B.length $ bs
  yield (ln `B.append` bs)

handleWithEncryption :: (Serialize a, MonadIO m) => B.ByteString -> a -> ConduitM a B.ByteString m ()
handleWithEncryption secret packet = do
  let bs = (encode packet)
  let ln = runPut (putVarInt (B.length $ bs))
  let encrypted = B.concat . fmap (encrypt secret) . breakup $ (ln `B.append` bs)
  yield encrypted
  where
    breakup :: B.ByteString -> [B.ByteString]
    breakup bs = if B.length bs > 16
                 then (B.take 16 bs):breakup (B.drop 16 bs)
                 else [bs]

handleWithBoth :: (Serialize a, Monad m) => B.ByteString -> a -> ConduitM a B.ByteString m ()
handleWithBoth secret bs = do
  let uncompressedBS = (encodeLazy bs)
  let dataLn = runPut . putVarInt . B.length . BL.toStrict $ uncompressedBS
  let compressedBS = BL.toStrict . Zlib.compress $ uncompressedBS
  let packetData = encrypt secret (dataLn `B.append` compressedBS)
  let packetLn = runPut . putVarInt . B.length $ packetData
  yield (packetLn `B.append` packetData)

handleHandshaking :: Logger -> Conduit (SBHandshaking,Maybe SBStatus) (StateT ProtocolState IO) SBStatus
handleHandshaking logger = awaitForever $ \handshake -> do
  liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show handshake
  case handshake of
    (SBHandshake _ _ _ ProtocolHandshake,_) -> do
      liftIO $ logMsg logger LvlDebug "Redundant handshake"
      return ()
    (SBHandshake _ _ _ ProtocolStatus,status) -> do
      liftIO $ logMsg logger LvlDebug "Switching protocol state to STATUS"
      lift $ State.put ProtocolStatus
      forM_ status yield
    (SBHandshake _ _ _ ProtocolLogin,_) -> do
      liftIO $ logMsg logger LvlDebug "Switching protocol state to LOGIN"
      lift $ State.put ProtocolLogin
      return ()
    (SBHandshake _ _ _ ProtocolPlay,_) -> do
      liftIO $ logMsg logger LvlDebug "Rejecting attempt to set protocol state to PLAY"
      return ()
    (SBLegacyServerListPing,_) -> do
      liftIO $ logMsg logger LvlDebug "Recieved LegacyServerListPing"
      return ()

handleStatus  :: Config -> Logger -> Conduit SBStatus (StateT ProtocolState IO) CBStatus
handleStatus config logger = awaitForever $ \status ->
      case status of
        SBRequest -> sendAndLog $
          CBResponse
            snapshotVersion
            (toEnum protocolVersion)
            0
            (toEnum . fromEnum . srvMaxPlayers $ config)
            (srvMotd config)
        SBPing payload -> sendAndLog $ CBPong payload
  where
    sendAndLog :: MonadIO m => CBStatus -> Conduit SBStatus m CBStatus
    sendAndLog packet = do
      liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show packet
      yield packet

handleLogin :: Logger -> Encryption -> TVar UserStore -> Conduit SBLogin (StateT Session IO) CBLogin
handleLogin logger encryption existingUsers = awaitForever $ \packet -> do
    liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show packet
    case packet of
      SBLoginStart username -> do
        session <- lift State.get
        thisUser <- liftIO $ registerUser existingUsers username
        lift $ State.modify $ \s -> s { sessionUsername = Just username }
        liftIO $ logMsg logger LvlDebug "Switching protocol state to PLAY"
        if sessionEncryptionIsEnabled session
          then do
            let encryptionRequest = CBEncryptionRequest "" (getCert encryption) (getVerifyToken encryption)
            liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show encryptionRequest
            yield encryptionRequest
          else do
            when (sessionCompressionIsEnabled session) $ sendAndLog $ CBSetCompression 0
            sendAndLog $ CBLoginSuccess (getUserUUID thisUser) (getUserName thisUser)

      SBEncryptionResponse sharedSecret token -> do
        liftIO $ logMsg logger LvlDebug "Got an encryption response!"
        session <- lift State.get
        sharedSecret' <- liftIO $ do
          eitherDecrypted <- decryptSafer (getPrivKey encryption) sharedSecret
          case eitherDecrypted of
            Left err -> error (show err)
            Right decrypted -> return decrypted
        token' <- liftIO $ do
          eitherDecrypted <- decryptSafer (getPrivKey encryption) token
          case eitherDecrypted of
            Left err -> error (show err)
            Right decrypted -> return decrypted
        if token' == (sessionVerifyToken session)
          then do
            lift $ State.modify $ \s ->
              s {sessionSharedSecret = Just sharedSecret'}
            thisUser <- liftIO $ registerUser existingUsers (fromJust $ sessionUsername session)
            when (sessionCompressionIsEnabled session) $ do
              sendAndLog $ CBSetCompression 0
              lift $ State.modify $ \s ->
                s { sessionSharedSecret = Just sharedSecret'
                  , sessionCompressionIsActive = True}
            sendAndLog $ CBLoginSuccess (getUserUUID thisUser) (getUserName thisUser)
          else do
            liftIO $ logMsg logger LvlError "Mismatching tokens!"
            return ()
  where
    sendAndLog :: MonadIO m => CBLogin -> Conduit SBLogin m CBLogin
    sendAndLog packet = do
      liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show packet
      yield packet

handlePlay  :: Config -> Logger -> WorldClock -> World -> TVar [Event] -> Conduit SBPlay (StateT Session IO) CBPlay
handlePlay config logger worldClock world history = do
  someUUID <- liftIO nextRandom
  sendAndLog $
        CBJoinGame
          2566
          (srvGameMode config)
          (srvDimension config)
          (srvDifficulty config)
          (srvMaxPlayers config)
          (T.pack . show $ srvWorldType config)
          True
  sendAndLog $ CBPluginMessage "MC|Brand" (encodeUtf8 "opensandbox")
  sendAndLog $ CBPluginMessage "REGISTER" (encodeUtf8 "MC|Brand")
  sendAndLog $ CBServerDifficulty (srvDifficulty config)
  sendAndLog $ CBSpawnPosition 0
  sendAndLog $ CBPlayerAbilities 0 1028443341 0
  sendAndLog $ CBHeldItemChange 0
  sendAndLog $ CBEntityStatus 32 AnimalInLove
  sendAndLog $ CBStatistics []
  sendAndLog $ CBPlayerListItem $
    PlayerListAdds
      [ PlayerListAdd someUUID "oldmanmike" [] Survival 0 Nothing
      ]
  sendAndLog $ CBPlayerPositionAndLook 0 4 0 0 0 0 777
  sendAndLog $ CBWorldBorder $
    Initialize 0 0 4723321873536909312 4723321873536909312 0 29999984 5 15
  worldAge <- liftIO $ getWorldAge worldClock
  worldTime <- liftIO $ getWorldTime worldClock
  sendAndLog $ CBTimeUpdate worldAge worldTime
  sendAndLog $ CBWindowItems 0 (V.replicate 46 (mkSlot (-1) 1 1 (NBT "" (ByteTag 0))))
  sendAndLog $ CBSetSlot (-1) (-1) (mkSlot (-1) 1 1 (NBT "" (ByteTag 0)))
  mapM_ (yield . CBChunkData) $ pullWorld world

  awaitForever $ \packet -> do
        liftIO $ threadDelay 10000
        liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show packet
        age <- liftIO $ getWorldAge worldClock
        t <- liftIO $ getWorldTime worldClock
        maybeOutgoing <- liftIO $ atomically $ (handle history age) packet
        forM_ maybeOutgoing yield
        -- Rewind
        when (mod t 1000 == 0) $ do
          past <- liftIO $ readTVarIO history
          forM_ past $ \event -> do
            liftIO $ logMsg logger LvlDebug $ "Undoing: " ++ show event
            liftIO $ threadDelay 10000
            yield . eventToCBPlay $ event
          liftIO $ atomically $ writeTVar history []
        when (mod t 20 == 0) $ sendAndLog $ CBTimeUpdate age t
        when (mod t 40 == 0) $ sendAndLog  $ CBKeepAlive 5346

  where
    sendAndLog :: MonadIO m => CBPlay -> Conduit SBPlay m CBPlay
    sendAndLog packet = do
      liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show packet
      yield packet

    handle :: TVar [Event] -> Int64 -> SBPlay -> STM (Maybe CBPlay)
    handle eventJournal age packet =
      case packet of
        SBTeleportConfirm {} -> return Nothing
        SBTabComplete txt shouldAssumeCommand _ ->
          if shouldAssumeCommand
             then case A.parseOnly assumedCommand txt of
                    Left _ -> return (Just $ CBTabComplete V.empty)
                    Right prefix -> do
                      let matches = filter (T.isPrefixOf prefix) availableCommands
                      return (Just $ CBTabComplete (V.fromList matches))
             else case A.parseOnly potentialCommand txt of
                    Left _ -> return (Just $ CBTabComplete V.empty)
                    Right prefix -> do
                      let matches = filter (T.isPrefixOf prefix) availableCommands
                      return (Just $ CBTabComplete (V.fromList matches))

        SBChatMessage message -> do
          past <- readTVar eventJournal
          writeTVar eventJournal $ (ChatMessage age message 0):past
          return (Just $ CBChatMessage (Chat message) 0)
        SBClientStatus {} -> return Nothing
        SBClientSettings {} -> return Nothing
        SBConfirmTransaction {} -> return Nothing
        SBEnchantItem {} -> return Nothing
        SBClickWindow {} -> return Nothing
        SBCloseWindow {} -> return Nothing
        SBPluginMessage {} -> return Nothing
        SBUseEntity {} -> return Nothing
        SBKeepAlive {} -> return Nothing
        SBPlayerPosition x y z onGround -> do
          past <- readTVar eventJournal
          case find isLatestPlayerPositionAndLook past of
            Nothing ->
              writeTVar eventJournal [(PlayerPositionAndLook age x y z 0 0 True)]
            Just (PlayerPositionAndLook _  _ _ _ yaw pitch _) ->
              writeTVar eventJournal $ (PlayerPositionAndLook age x y z yaw pitch onGround):past
            Just _ -> undefined
          return Nothing

        SBPlayerPositionAndLook x y z yaw pitch onGround -> do
          past <- readTVar eventJournal
          writeTVar eventJournal $ (PlayerPositionAndLook age x y z yaw pitch onGround):past
          return Nothing

        SBPlayerLook yaw pitch onGround -> do
          past <- readTVar eventJournal
          case find isLatestPlayerPositionAndLook past of
            Nothing -> do
              writeTVar eventJournal [(PlayerPositionAndLook age 0 0 0 yaw pitch onGround)]
              return Nothing
            Just (PlayerPositionAndLook _ x0 y0 z0 _ _ _) -> do
              writeTVar eventJournal $ (PlayerPositionAndLook age x0 y0 z0 yaw pitch onGround):past
              return Nothing
            Just _ -> undefined

        SBPlayer {} -> return Nothing
        SBVehicleMove {} -> return Nothing
        SBSteerBoat {} -> return Nothing
        SBPlayerAbilities {} -> return Nothing
        SBPlayerDigging {} -> return Nothing
        SBEntityAction {} -> return Nothing
        SBSteerVehicle {} -> return Nothing
        SBResourcePackStatus {} -> return Nothing
        SBHeldItemChange {} -> return Nothing
        SBCreativeInventoryAction {} -> return Nothing
        SBUpdateSign {} -> return Nothing
        SBAnimation {} -> return Nothing
        SBSpectate {} -> return Nothing
        SBPlayerBlockPlacement {} -> return Nothing
        SBUseItem {} -> return Nothing
      where
        isLatestPlayerPositionAndLook PlayerPositionAndLook{} = True
        isLatestPlayerPositionAndLook _ = False

assumedCommand :: A.Parser T.Text
assumedCommand = do
  A.skip (=='/')
  A.takeText

potentialCommand :: A.Parser T.Text
potentialCommand = do
  _ <- A.char '/'
  txt <- A.takeText
  return $ T.cons '/' txt

availableCommands :: [T.Text]
availableCommands = ["/rewind","/help"]

eventToCBPlay :: Event -> CBPlay
eventToCBPlay (PlayerPositionAndLook age x y z yaw pitch _) =
  CBPlayerPositionAndLook x y z yaw pitch 0 (fromEnum age)
eventToCBPlay (ChatMessage _ message position) =
  CBChatMessage (Chat message) position
