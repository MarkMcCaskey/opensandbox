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
  (conn,something) <- flip runStateT Handshake $ packetSource app $$+ processStatus srv app
  print $ "Protocol state is: " ++ show something


processStatus :: Server -> AppData -> Sink B.ByteString (StateT ProtocolState IO) ()
processStatus srv app = deserializeStatus =$= handleStatus srv =$= serializeStatus =$= packetSink app

processLogin :: Server -> AppData -> Sink B.ByteString (StateT ProtocolState IO) ()
processLogin srv app = deserializeLogin =$= handleLogin srv =$= serializeLogin =$= packetSink app

processPlay :: Server -> AppData -> Sink B.ByteString (StateT ProtocolState IO) ()
processPlay srv app = deserializePlay =$= handlePlay srv =$= serializePlay =$= packetSink app


packetSource :: AppData -> Source (StateT ProtocolState IO) B.ByteString
packetSource app = transPipe lift $ appSource app

packetSink :: AppData -> Sink B.ByteString (StateT ProtocolState IO) ()
packetSink app = transPipe lift $ appSink app


deserializeStatus :: Conduit B.ByteString (StateT ProtocolState IO) ServerBoundStatus
deserializeStatus = conduitGet (S.get :: S.Get ServerBoundStatus)

serializeStatus :: Conduit ClientBoundStatus (StateT ProtocolState IO) B.ByteString
serializeStatus = conduitPut (S.put :: S.Putter ClientBoundStatus)


deserializeLogin :: Conduit B.ByteString (StateT ProtocolState IO) ServerBoundLogin
deserializeLogin = conduitGet (S.get :: S.Get ServerBoundLogin)

serializeLogin :: Conduit ClientBoundLogin (StateT ProtocolState IO) B.ByteString
serializeLogin = conduitPut (S.put :: S.Putter ClientBoundLogin)


deserializePlay :: Conduit B.ByteString (StateT ProtocolState IO) ServerBoundPlay
deserializePlay = conduitGet (S.get :: S.Get ServerBoundPlay)

serializePlay :: Conduit ClientBoundPlay (StateT ProtocolState IO) B.ByteString
serializePlay = conduitPut (S.put :: S.Putter ClientBoundPlay)


handleStatus :: Server -> Conduit ServerBoundStatus (StateT ProtocolState IO) ClientBoundStatus
handleStatus srv = do
  maybeHandshake <- await
  case maybeHandshake of
    Just (ServerBoundHandshake _ _ _ 1) ->
      do  maybePingStart <- await
          lift $ put Status
          let version = srvVersion srv
          let players = srvPlayers srv
          let maxPlayers = srvMaxPlayers srv
          let motd = srvMotd srv
          let status = buildStatus version players maxPlayers motd
          yield $ ClientBoundResponse . BL.toStrict . Aeson.encode $ status
          maybePing <- await
          case maybePing of
            Just (ServerBoundPing payload) -> yield (ClientBoundPong payload)
            Nothing -> return ()
    Just (ServerBoundHandshake _ _ _ 2) -> lift $ put Login >> return ()
    Just _ -> return ()
    Nothing -> return ()

handleLogin :: Server -> Conduit ServerBoundLogin (StateT ProtocolState IO) ClientBoundLogin
handleLogin srv = undefined

handlePlay :: Server -> Conduit ServerBoundPlay (StateT ProtocolState IO) ClientBoundPlay
handlePlay srv = undefined
