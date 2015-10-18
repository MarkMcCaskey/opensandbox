{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import            OpenSandbox
import            OpenSandbox.Minecraft.Protocol.Handshaking
import            Control.Monad
import qualified  Data.Text as T
import            GHC.Generics
import            Network.Socket hiding (send,recv)
import            Network.Socket.ByteString
import            Data.Aeson
import qualified  Data.Attoparsec.ByteString as P
import qualified  Data.Binary as Bin
import qualified  Data.Binary.Get as BinG
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import            Data.Word


main :: IO ()
main = do
    putStrLn "Ops are: "
    ops <- readOps "ops.json"
    case ops of
        Left err -> putStrLn err
        Right opsList -> print opsList

    putStrLn "Users are: "
    users <- readUsers "usercache.json"
    case users of
        Left err -> putStrLn err
        Right userlist -> do
            print userlist
            writeUsers "test.json" userlist

    putStrLn "Whitelisted Users are: "
    whitelist <- readWhiteList "whitelist.json"
    case whitelist of
        Left err -> putStrLn err
        Right whitelist -> print whitelist

    putStrLn "Banned IPs are: "
    bannedIPs <- readBannedIPs "banned-ips.json"
    case bannedIPs of
        Left err -> putStrLn err
        Right bannedIPs -> print bannedIPs

    putStrLn "Banned Players are: "
    bannedPlayers <- readBannedPlayers "banned-players.json"
    case bannedPlayers of
        Left err -> putStrLn err
        Right bannedPlayers -> print bannedPlayers

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
  --print $ BinG.runGet Bin.get packet
  --putStrLn $ show $ B.unpack $ B.cons (15 :: Word8) (B.cons (0 :: Word8) (BL.toStrict (encode testResponse)))
  let response = BL.toStrict (encode testResponse)
  let outgoing = B.cons (111 :: Word8) (B.cons (0 :: Word8) (B.cons (109 :: Word8) response))
  putStrLn $ show outgoing
  send sock outgoing
  sClose sock


{-
routePacket :: Packet -> IO ()
routePacket packet = do
  case (fromEnum $ B.index packet 1) of
    0 -> handshake
-}


testResponse :: Response
testResponse = Response
  (Version "1.8.7" 47)
  (Players 20 0)
  ("A Minecraft Server")


data Response = Response
  { version       :: Version
  , players       :: Players
  , description   :: T.Text
  } deriving (Generic,Show)


instance ToJSON Response
instance FromJSON Response


data Version = Version
  { name      :: T.Text
  , protocol  :: Int
  } deriving (Generic,Show)


instance ToJSON Version
instance FromJSON Version


data Players = Players
  { max     :: Int
  , online  :: Int
  } deriving (Generic,Show)


instance ToJSON Players
instance FromJSON Players
