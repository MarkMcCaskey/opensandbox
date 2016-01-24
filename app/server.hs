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

import qualified  Data.Aeson as Aeson
import qualified  Data.ByteString.Lazy as BL

import            Control.Monad.Catch
import            Control.Monad.IO.Class
import            Control.Monad.Trans.Class
import            Control.Monad.Trans.State.Lazy
import qualified  Data.ByteString as B
import            Data.Conduit
import            Data.Conduit.Cereal
import            Data.Conduit.Network
import qualified  Data.Serialize as S
import            Data.Text.Encoding
import            Data.UUID
import            Data.UUID.V4
import            Network.Socket hiding (send,recv)
import            Network.Socket.ByteString
import            OpenSandbox

myVersion :: String
myVersion = "16w03a"

myBackupPath :: FilePath
myBackupPath = "backup"

myLogPath :: FilePath
myLogPath = "log"

mySrvPath :: FilePath
mySrvPath = "."

myPort :: Int
myPort = 25567

main :: IO ()
main = withSocketsDo $ do
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
    runTCPServer (serverSettings 25567 "*") $ \app -> appSource app $$ deserialize =$= handler srv =$= serialize =$= appSink app


deserialize :: Conduit B.ByteString IO ServerBoundPacket
deserialize = do
  (liftIO $ print "Testing") >> conduitGet (S.get :: S.Get ServerBoundPacket)

serialize :: Conduit ClientBoundPacket IO B.ByteString
serialize = conduitPut (S.put :: S.Putter ClientBoundPacket)

handler :: Server -> Conduit ServerBoundPacket IO ClientBoundPacket
handler srv = do
  maybeHandshake <- await
  case maybeHandshake of
    Just (SBS (Handshake _ _ _ 1)) ->
      do  maybePingStart <- await
          let version = srvVersion srv
          let players = srvPlayers srv
          let maxPlayers = srvMaxPlayers srv
          let motd = srvMotd srv
          yield (CBS (Response $ BL.toStrict $ Aeson.encode $ buildStatus version players maxPlayers motd))
          maybePing <- await
          case maybePing of
            Just (SBS (Ping payload)) -> yield (CBS (Pong payload))
            Nothing -> return ()
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
    Just _ -> return ()
    Nothing -> return ()
