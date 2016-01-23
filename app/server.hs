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
import qualified  Data.ByteString as B
import            Data.Conduit
import            Data.Conduit.Cereal
import            Data.Conduit.Network
import            Data.Serialize
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
    runTCPServer (serverSettings 25567 "*") $ \app -> appSource app $$ deserializeStatus =$= handler srv =$= serializeStatus =$= appSink app

deserializeStatus :: MonadThrow m => Conduit B.ByteString m ServerBoundStatus
deserializeStatus = conduitGet (get :: Get ServerBoundStatus)

serializeStatus :: MonadThrow m => Conduit ClientBoundStatus m B.ByteString
serializeStatus = conduitPut (put :: Putter ClientBoundStatus)

handler :: Server -> Conduit ServerBoundStatus IO ClientBoundStatus
handler srv = do
  maybeHandshake <- await
  case maybeHandshake of
    Just (Handshake _ _ _ 1) ->
      do  maybePingStart <- await
          let version = srvVersion srv
          let players = srvPlayers srv
          let maxPlayers = srvMaxPlayers srv
          let motd = srvMotd srv
          yield (Response $ BL.toStrict $ Aeson.encode $ buildStatus version players maxPlayers motd)
          maybePing <- await
          case maybePing of
            Just (Ping payload) -> yield (Pong payload)
            Nothing -> return ()
    {-
    Just (Handshake _ _ _ 2) ->
    do    maybeLoginStart <- await
          case maybeLoginStart of
            Just loginStart ->
              do  let someUsername = decodeUtf8 $ B.drop 3 loginStart
                  someUUID <- nextRandom
                  let someUser = User someUUID someUsername Nothing Nothing
                  yield (ClientBoundLoginSuccess someUUID someUser
            Nothing -> return ()
    -}
    Just _ -> return ()
    Nothing -> return ()
