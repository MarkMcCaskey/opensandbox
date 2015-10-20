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
  forkIO (runConn conn)
  mainLoop sock


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  packet <- recv sock 254
  putStrLn "==================================================================="
  putStrLn "|                        << Packet Report >>                      |"
  putStrLn "==================================================================="
  putStrLn "[Raw]"
  print $ B.unpack packet
  putStrLn "==================================================================="
  putStrLn "[Parsed]"
  print $ (decode (BL.fromStrict packet) :: ServerBoundHandshake)
  putStrLn "==================================================================="
  putStrLn "///////////////////////////////////////////////////////////////////"
  let response = BL.toStrict (Aeson.encode testResponse)
  let outgoing = B.cons (111 :: Word8) (B.cons (0 :: Word8) (B.cons (109 :: Word8) response))
  send sock outgoing
  sClose sock


testResponse :: Response
testResponse = Response
  (Version "15w42a" 79)
  (Players 20 0)
  (Description "A Minecraft Server")
