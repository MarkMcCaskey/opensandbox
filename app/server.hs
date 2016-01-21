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
import qualified  Data.ByteString as B
import            Data.Conduit
import            Data.Conduit.Cereal
import            Data.Conduit.Network
import            Data.Conduit.TMChan
import            Data.Serialize
import            Control.Concurrent.STM
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

myPort :: PortNumber
myPort = 25567

sendPacket :: Client -> ClientBoundPacket -> STM ()
sendPacket Client{..} packet = writeTMChan clientChan packet

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
    runTCPServer (serverSettings 25567 "*") $ \app -> appSource app $$ deserializeStatus =$= printSink

deserializeStatus :: MonadThrow m => Conduit B.ByteString m ServerBoundStatus
deserializeStatus = conduitGet (get :: Get ServerBoundStatus)

printSink :: Show i => Sink i IO ()
printSink = awaitForever $ liftIO . print

route :: Server -> Socket -> Either String ServerBoundStatus -> IO ()
route srv sock (Right PingStart)
  = putStrLn "--> Routing PingStart" -- >> (recv sock 10 >>= \x -> send sock x >> return ())
route srv sock (Right (Ping payload))
  = putStrLn "--> Routing Ping" >> (send sock $ encode (Ping payload)) >> return ()
route srv sock (Right Request)
  = putStrLn "--> Routing Request" >> return ()
route srv sock (Right (Handshake _ _ _ 1))
  = putStrLn "--> Routing Status Handshake" >> runStatus srv sock
route srv sock (Right (Handshake _ _ _ 2))
  = putStrLn "--> Routing Login Handshake" >> runLogin srv sock
route srv sock (Right (Handshake _ _ _ _))
  = putStrLn "Error: Unknown state!"
route srv sock (Left err)
  = putStrLn $ "Error: " ++ err
