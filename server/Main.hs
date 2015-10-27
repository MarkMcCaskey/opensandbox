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


mcPort    = 25567
mcVersion = "15w43c"
mcSrvPath = "."
mcWorld   = "world"


main :: IO ()
main = do
    print responsePacket
    putStrLn "Welcome to OpenSandbox Server!"
    putStrLn "Loading OpenSandbox properties..."
    putStrLn $ "Starting minecraft server version " ++ mcVersion
    putStrLn "Loading properties"
    loadMCServerProperties mcSrvPath
    putStrLn "Default game type: SURVIVAL"
    putStrLn "Generating key pair"
    gen <- newGenIO :: IO SystemRandom
    let (pubKey, privKey, _) = generateKeyPair gen 1024
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
    routeHandshake conn (decode (BL.fromStrict packet) :: ServerBoundHandshake)
    mainLoop sock


routeHandshake :: Socket -> ServerBoundHandshake -> IO ()
routeHandshake sock (Handshake _ _ _ 1) = runServerList sock
routeHandshake sock (Handshake _ _ _ 2) = runLogin sock
routeHandshake sock (Handshake _ _ _ _) = putStrLn "Error: Unknown state!"
routeHandshake sock _                   = putStrLn "Error: Unknown handshake!"


runServerList :: Socket -> IO ()
runServerList sock = do
    putStrLn "================================================================="
    putStrLn "|                   << Packet Report Begin >>                   |"
    putStrLn "================================================================="
    let response = BL.toStrict (Aeson.encode testResponse)
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


testResponse :: Response
testResponse = Response
    (Version (T.pack mcVersion) 82)
    (Players 20 0)
    (Description "A Minecraft Server")
