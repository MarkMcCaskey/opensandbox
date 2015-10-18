{-# LANGUAGE OverloadedStrings #-}
module Main where

import            OpenSandbox
import            OpenSandbox.Minecraft.Protocol
import qualified  Data.Aeson as Aeson
import qualified  Data.Text as T
import            GHC.Generics
import            Network.Socket hiding (send,recv)
import            Network.Socket.ByteString
import qualified  Data.Attoparsec.ByteString as P
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import            Data.Word


main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 25567 iNADDR_ANY)
    listen sock 1
    mainLoop sock


mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  runConn conn
  mainLoop sock


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  packet <- recv sock 254
  putStrLn "[Raw]"
  putStrLn $ show $ B.unpack packet
  putStrLn "[Parsed]"
  P.parseTest handshake packet
  let response = BL.toStrict (Aeson.encode testResponse)
  let outgoing = B.cons (111 :: Word8) (B.cons (0 :: Word8) (B.cons (109 :: Word8) response))
  putStrLn $ show outgoing
  send sock outgoing
  sClose sock


testResponse :: Response
testResponse = Response
  (Version "1.8.7" 47)
  (Players 20 0)
  ("A Minecraft Server")
