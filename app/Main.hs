{-# LANGUAGE OverloadedStrings #-}
module Main where


import            OpenSandbox
import            OpenSandbox.Minecraft.Protocol
import            Control.Concurrent
import qualified  Data.Aeson as Aeson
import qualified  Data.Text as T
import            GHC.Generics
import            Network.Socket hiding (send,recv)
import            Network.Socket.ByteString
import            Data.Binary
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import            Data.Word


data ClientBoundLogin
  = Disconnect T.Text
  | EncryptionRequest B.ByteString VarInt B.ByteString VarInt B.ByteString
  | LoginSuccess B.ByteString B.ByteString
  deriving (Show,Eq,Read)

data ServerBoundLogin
  = LoginStart B.ByteString
  | EncryptionResponse VarInt B.ByteString VarInt B.ByteString
  deriving (Show,Eq,Read)

main :: IO ()
main = do
    let port = 25567
    putStrLn ("Starting Minecraft Server on " ++ (show port))
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    listen sock 1
    mainLoop sock


mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  runServerList conn
  mainLoop sock

runServerList :: (Socket, SockAddr) -> IO ()
runServerList (sock, _) = do
  maybeHandshake <- recv sock 254
  putStrLn "==================================================================="
  putStrLn "|                    << Packet Report Begin >>                    |"
  putStrLn "==================================================================="
  putStrLn "[Raw Handshake]"
  print maybeHandshake
  print $ B.unpack maybeHandshake
  putStrLn "==================================================================="
  putStrLn "[Parsed]"
  print $ (decode (BL.fromStrict maybeHandshake) :: ServerBoundHandshake)
  let response = BL.toStrict (Aeson.encode testResponse)
  let outgoing = B.cons (121 :: Word8) (B.cons (0 :: Word8) (B.cons (119 :: Word8) response))
  send sock outgoing
  putStrLn "==================================================================="
  putStrLn "///////////////////////////////////////////////////////////////////"
  putStrLn "==================================================================="
  ping <- recv sock 254
  putStrLn "[Raw Ping]"
  print ping
  print $ B.unpack ping
  maybePing sock ping
  putStrLn "==================================================================="
  putStrLn "///////////////////////////////////////////////////////////////////"
  putStrLn "==================================================================="
  putStrLn "|                     << Packet Report End >>                     |"
  putStrLn "==================================================================="
  sClose sock

login :: Socket -> IO ()
login sock = do
  handshake <- recv sock 254
  loginStart <- recv sock 254
  print $ (decode (BL.fromStrict packet) :: ServerBoundHandshake)
  let encryptionRequest =
  send sock encryptionRequest
  recv



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
  (Version "15w42a" 79)
  (Players 20 0)
  (Description "A Minecraft Server")

