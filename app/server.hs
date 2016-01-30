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

import            Control.Monad.Catch
import            Control.Monad.IO.Class
import            Control.Monad.Trans.Class
import            Control.Monad.Trans.State.Lazy
import qualified  Data.Aeson as Aeson
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import            Data.Conduit
import            Data.Conduit.Cereal
import            Data.Conduit.Network
import qualified  Data.Serialize as S
import            Data.Text.Encoding
import            Data.UUID
import            Data.UUID.V4
import            OpenSandbox

myVersion :: String
myVersion = "16w04a"

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
    putStrLn "Welcome to the OpenSandbox Minecraft Server!"
    let config = defaultConfig
    let port = myPort
    putStrLn $ "Starting minecraft server version " ++ show myVersion
    putStrLn $ "Default game type: " ++ show (mcLevelType config)
    maybeEncryption <- configEncryption config
    maybeCompression <- configCompression config
    putStrLn $ "Starting Minecraft server on " ++ show port
    putStrLn $ "Preparing level " ++ show (mcLevelName config)
    putStrLn "Done!"
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
    runTCPServer (serverSettings myPort "*") $ runOpenSandbox srv


runOpenSandbox :: Server -> AppData -> IO ()
runOpenSandbox srv app = do
  (conn,_) <- packetSource app $$+ processStatus srv app
  return ()


processStatus :: (MonadIO m, MonadThrow m) => Server -> AppData -> Sink B.ByteString m ()
processStatus srv app = deserializeStatus =$= handleStatus srv =$= serializeStatus =$= packetSink app

processLogin :: (MonadIO m, MonadThrow m) => Server -> AppData -> Sink B.ByteString m ()
processLogin srv app = deserializeLogin =$= handleLogin srv =$= serializeLogin =$= packetSink app

processPlay :: (MonadIO m, MonadThrow m) => Server -> AppData -> Sink B.ByteString m ()
processPlay srv app = deserializePlay =$= handlePlay srv =$= serializePlay =$= packetSink app


packetSource :: (MonadIO m, MonadThrow m) => AppData -> Source m B.ByteString
packetSource app = appSource app

packetSink :: (MonadIO m, MonadThrow m) => AppData -> Sink B.ByteString m ()
packetSink app = appSink app


deserializeStatus :: (MonadIO m, MonadThrow m) => Conduit B.ByteString m ServerBoundStatus
deserializeStatus = conduitGet (S.get :: S.Get ServerBoundStatus)

serializeStatus :: (MonadIO m, MonadThrow m) => Conduit ClientBoundStatus m B.ByteString
serializeStatus = conduitPut (S.put :: S.Putter ClientBoundStatus)


deserializeLogin :: (MonadIO m, MonadThrow m) => Conduit B.ByteString m ServerBoundLogin
deserializeLogin = conduitGet (S.get :: S.Get ServerBoundLogin)

serializeLogin :: (MonadIO m, MonadThrow m) => Conduit ClientBoundLogin m B.ByteString
serializeLogin = conduitPut (S.put :: S.Putter ClientBoundLogin)


deserializePlay :: (MonadIO m, MonadThrow m) => Conduit B.ByteString m ServerBoundPlay
deserializePlay = conduitGet (S.get :: S.Get ServerBoundPlay)

serializePlay :: (MonadIO m, MonadThrow m) => Conduit ClientBoundPlay m B.ByteString
serializePlay = conduitPut (S.put :: S.Putter ClientBoundPlay)


handleStatus :: MonadIO m => Server -> Conduit ServerBoundStatus m ClientBoundStatus
handleStatus srv = do
  maybeHandshake <- await
  case maybeHandshake of
    Just (Handshake _ _ _ 1) ->
      do  maybePingStart <- await
          let version = srvVersion srv
          let players = srvPlayers srv
          let maxPlayers = srvMaxPlayers srv
          let motd = srvMotd srv
          let status = buildStatus version players maxPlayers motd
          yield $ Response . BL.toStrict . Aeson.encode $ status
          maybePing <- await
          case maybePing of
            Just (Ping payload) -> yield (Pong payload)
            Nothing -> return ()
    {-
    Just (SBS (Handshake _ _ _ 2)) ->
      do  maybeLoginStart <- await
          case maybeLoginStart of
            Just (SBL (ServerBoundLoginStart username)) ->
              do  let someUsername = decodeUtf8 $ B.drop 3 username
                  someUUID <- liftIO $ nextRandom
                  let someUser = User someUUID someUsername Nothing Nothing
                  yield (CBL (ClientBoundLoginSuccess (BL.toStrict.toByteString $ someUUID) (B.drop 3 username)))
            Just _ -> return ()
            Nothing -> return ()
    -}
    Just _ -> return ()
    Nothing -> return ()

handleLogin :: MonadIO m => Server -> Conduit ServerBoundLogin m ClientBoundLogin
handleLogin srv = undefined

handlePlay :: MonadIO m => Server -> Conduit ServerBoundPlay m ClientBoundPlay
handlePlay srv = undefined
