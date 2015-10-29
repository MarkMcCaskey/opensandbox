{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : Main
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module Main where


import            Codec.Crypto.RSA
import            Control.Concurrent
import            Crypto.PubKey.RSA
import            Crypto.Random
import qualified  Data.Aeson as Aeson
import            Data.ASN1.BitArray
import            Data.ASN1.Types
import            Data.Binary
import            Data.Bytes.VarInt
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.Text as T
import            Data.Word
import            GHC.Generics
import            Network.Socket hiding (send,recv)
import            Network.Socket.ByteString
import            OpenSandbox
import            OpenSandbox.Minecraft.Protocol
import            System.Random


mcPort :: PortNumber
mcPort = 25567


mcVersion :: T.Text
mcVersion = "15w43c"


mcSrvPath :: FilePath
mcSrvPath = "."


mcWorld :: T.Text
mcWorld = "world"


mcMaxPlayers :: Int
mcMaxPlayers = 20


mcMotd :: T.Text
mcMotd = "A Minecraft Server"


main :: IO ()
main = do
    putStrLn "Welcome to OpenSandbox Server!"
    putStrLn "Loading OpenSandbox properties..."
    putStrLn $ "Starting minecraft server version " ++ (show mcVersion)
    putStrLn "Loading properties"
    loadMCServerProperties mcSrvPath
    putStrLn "Default game type: SURVIVAL"
    putStrLn "Generating key pair"
    entropy <- createEntropyPool
    let gen = cprgCreate entropy :: SystemRNG
    let (pubKey,privKey) = fst $ generate gen 128 63
    putStrLn $ "Starting Minecraft server on " ++ (show mcPort)
    putStrLn $ "Preparing level " ++ (show mcWorld)
    putStrLn "Done!"
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet mcPort iNADDR_ANY)
    listen sock 1
    mainLoop sock


responsePacket = [ (Start Sequence)
                 , (Start Sequence)
                 , (OID [1,2,840,113549,1,1,1])
                 , Null
                 , (End Sequence)
                 , (BitString (BitArray 0 "test"))
                 , (End Sequence)
                 , (Start Sequence)
                 , (IntVal 52) -- Just a temp val
                 , (IntVal 63) -- Just a temp val
                 , (End Sequence)]


loadMCServerProperties :: FilePath -> IO ()
loadMCServerProperties path = return ()


mainLoop :: Socket -> IO ()
mainLoop sock = do
    (conn,_) <- accept sock
    packet <- recv conn 256
    routeHandshake conn (decode (BL.fromStrict packet) :: ServerBoundStatus)
    mainLoop sock


routeHandshake :: Socket -> ServerBoundStatus -> IO ()
routeHandshake sock (Handshake _ _ _ 1) = runStatus sock
routeHandshake sock (Handshake _ _ _ 2) = runLogin sock
routeHandshake sock (Handshake _ _ _ _) = putStrLn "Error: Unknown state!"
routeHandshake sock _                   = putStrLn "Error: Unknown handshake!"


runStatus :: Socket -> IO ()
runStatus sock = do
    putStrLn "================================================================="
    putStrLn "|                   << Packet Report Begin >>                   |"
    putStrLn "================================================================="
    let response = BL.toStrict $ Aeson.encode $ buildResponse mcVersion 0 20 mcMotd
    let response' = (B.cons (0 :: Word8) (B.cons (fromIntegral $ B.length response :: Word8) response))
    let outgoing = B.cons (fromIntegral $ B.length response' :: Word8) response'
    send sock outgoing
    putStrLn "================================================================="
    putStrLn "/////////////////////////////////////////////////////////////////"
    putStrLn "================================================================="
    ping <- recv sock 256
    putStrLn "[Raw Ping]"
    print ping
    print $ B.unpack ping
    maybePing sock ping
    putStrLn "================================================================="
    putStrLn "/////////////////////////////////////////////////////////////////"
    putStrLn "================================================================="
    putStrLn "|                    << Packet Report End >>                    |"
    putStrLn "================================================================="
    sClose sock


runLogin :: Socket -> IO ()
runLogin sock = do
    loginStart <- recv sock 254
    print $ (B.drop 3 loginStart) `B.append` " is logging in..."
    let encryptRequest = undefined
    send sock encryptRequest
    encryptResponse <- recv sock 512
    let loginSuccess = undefined
    send sock loginSuccess
    let setCompression = undefined
    send sock setCompression
    sClose sock


maybePing :: Socket -> B.ByteString -> IO ()
maybePing sock maybePing = do
    if ((B.length maybePing == 10) && ((B.index maybePing 1) == 1))
        then do
              send sock maybePing
              return ()
        else do
              packet <- recv sock 254
              send sock packet
              return ()
