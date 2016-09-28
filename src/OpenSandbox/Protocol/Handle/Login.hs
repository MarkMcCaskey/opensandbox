{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Handle.Login
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Handle.Login
  ( handleLogin
  , serializeLogin
  ) where

import Crypto.PubKey.RSA.PKCS15 (decryptSafer)
import qualified Codec.Compression.Zlib as Zlib
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.State.Lazy as State
import Data.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Serialize
import qualified Data.Text as T
import OpenSandbox.Logger
import OpenSandbox.Protocol.Encryption
import OpenSandbox.Protocol.Packet (CBLogin(..),SBLogin(..))
import OpenSandbox.Protocol.Types (ProtocolState(..),Session(..),putVarInt)
import OpenSandbox.User

logMsg :: Logger -> Lvl -> String -> IO ()
logMsg logger lvl msg = logIO logger "OpenSandbox.Protocol.Handle.Login" lvl (T.pack msg)

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

serializeLogin :: Conduit CBLogin (StateT Session IO) B.ByteString
serializeLogin = do
  maybePacket <- await
  session <- lift State.get
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
  let bs = encode packet
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
  let uncompressedBS = encodeLazy bs
  let dataLn = runPut . putVarInt . B.length . BL.toStrict $ uncompressedBS
  let compressedBS = BL.toStrict . Zlib.compress $ uncompressedBS
  let packetData = encrypt secret (dataLn `B.append` compressedBS)
  let packetLn = runPut . putVarInt . B.length $ packetData
  yield (packetLn `B.append` packetData)
