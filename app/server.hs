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

myVersionID :: Int
myVersionID = 97

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
    runTCPServer (serverSettings 25567 "*")
      $ \app -> flip evalStateT 0
      $ packetSource app
      $$ deserialize
      =$= handler srv
      =$= serialize
      =$= packetSink app

packetSource :: AppData -> Source (StateT Int IO) B.ByteString
packetSource app = transPipe lift $ appSource app

packetSink :: AppData -> Sink B.ByteString (StateT Int IO) ()
packetSink app = transPipe lift $ appSink app

deserialize :: Conduit B.ByteString (StateT Int IO) ServerBoundStatus
deserialize = conduitGet (S.get :: S.Get ServerBoundStatus)

serialize :: Conduit ClientBoundStatus (StateT Int IO) B.ByteString
serialize = conduitPut (S.put :: S.Putter ClientBoundStatus)

handler :: Server -> Conduit ServerBoundStatus (StateT Int IO) ClientBoundStatus
handler srv = do
  maybeHandshake <- await
  case maybeHandshake of
    Just (Handshake _ _ _ 1) ->
      do  maybePingStart <- await
          let version = srvVersion srv
          let versionID = myVersionID
          let players = srvPlayers srv
          let maxPlayers = srvMaxPlayers srv
          let motd = srvMotd srv
          let status = buildStatus version versionID players maxPlayers motd
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
