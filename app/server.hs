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
import qualified  Data.ByteString as B
import            Data.Conduit
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
    runTCPServer (serverSettings 25567 "*") $ \app -> appSource app $$ printPacketSink
    --runListener srv port serverLoop

printPacketSink :: Sink B.ByteString IO ()
printPacketSink = do
    maybePacket <- await
    case maybePacket of
      Nothing -> return ()
      Just packet -> do
        liftIO $ print $ B.unpack packet
        printPacketSink

runListener :: Server -> PortNumber -> (Server -> Socket -> IO ()) -> IO ()
runListener srv port handler = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    listen sock 1
    handler srv sock

serverLoop :: Server -> Socket -> IO ()
serverLoop srv sock = do
    (conn,_) <- accept sock
    packet <- recv conn 256
    mapM_ (route srv conn . (decode :: B.ByteString -> Either String ServerBoundStatus)) (splitPacket packet)
    serverLoop srv sock

splitPacket :: B.ByteString -> [B.ByteString]
splitPacket "" = []
splitPacket packet = if (B.length packet /=  1 + (fromIntegral $ B.head packet))
                        then do let (x,xs) = B.splitAt (1 + (fromIntegral $ B.head packet)) packet
                                let xs' = splitPacket xs
                                x:xs'
                        else [packet]

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
