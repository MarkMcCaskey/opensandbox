{-# LANGUAGE DeriveGeneric #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Status
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Status
    ( ClientBoundStatus (..)
    , ServerBoundStatus (..)
    , StatusPayload
    , Version
    , Players
    , Description
    , runStatus
    , buildStatus
    , buildResponse
    ) where


import qualified  Data.Aeson as Aeson
import qualified  Data.ByteString.Char8 as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.Text as T
import            Data.Bits
import            Data.Serialize
import            Data.Serialize.Get
import            Data.Serialize.Put
import            Data.Word
import            GHC.Generics
import            Network.Socket hiding (send,recv)
import            Network.Socket.ByteString

import            OpenSandbox.Server

runStatus :: Server -> Socket -> IO ()
runStatus srv sock = do
    startPing <- recv sock 2
    let response1 = Response $ BL.toStrict $ Aeson.encode $ buildStatus (srvVersion srv) (srvPlayers srv) (srvMaxPlayers srv) (srvMotd srv)
    let outgoing1 = runPut $ put response1
    send sock outgoing1
    ping <- recv sock 10
    send sock ping
    sClose sock

buildResponse :: StatusPayload -> ClientBoundStatus
buildResponse s = Response $ BL.toStrict $ Aeson.encode s

{-
data Status = Status
  { srvCurrentVersion :: !T.Text
  , srvCurrentPlayers :: !Int
  , srvMaxPlayers     :: !Int
  , srvMotd           :: !T.Text
  } deriving (Show,Eq)
-}
data ServerBoundStatus
    = Handshake Word8 B.ByteString Word16 Word8
    | PingStart
    | Ping Word64
    | Request
    deriving (Show,Eq,Read)


data ClientBoundStatus
    = Response B.ByteString
    | Pong Word64
    deriving (Show,Eq,Read)


instance Serialize ServerBoundStatus where
  put (Handshake v a p s) = do
    put (fromIntegral $ 6 + B.length a :: Word8)
    put (0 :: Word8)
    put v
    put (fromIntegral $ B.length a :: Word8)
    putByteString a
    put p
    put s
  put PingStart = do
    put (1 :: Word8)
    put (0 :: Word8)
  put (Ping payload) = do
    put (fromIntegral $ 2 + 8 :: Word8)
    put (1 :: Word8)
    putWord64be payload

  get = do
    len <- getWord8
    packetID <- getWord8
    case packetID of
      0 -> case len of
            1 -> return PingStart
            _ -> Handshake <$> getWord8 <*> (getWord8 >>= (getByteString . fromIntegral)) <*> getWord16be <*> getWord8
      1 -> Ping <$> (get :: Get Word64)
      {-
      1 -> case len of
            10 -> Ping <$> (get :: Get Word64)
            254 -> return Request
            _ -> fail ("Unrecognized packet with Packet ID 1! It seems to be " ++ show len)
            -}
      _ -> fail "Unrecognized packet!"


instance Serialize ClientBoundStatus where
  put (Response payload) = do
    put (fromIntegral $ 2 + B.length payload :: Word8)
    put (0 :: Word8)
    put (fromIntegral $ B.length payload :: Word8)
    putByteString payload
  put (Pong payload) = do
    put (fromIntegral $ 1 + 8 :: Word8) -- (NOTE) This is probably a bug from Mojang
    put (1 :: Word8)
    put payload

  get = do
    _ <- getWord8
    packetID <- getWord8
    case packetID of
      0 -> Response <$> (getWord8 >>= (getByteString . fromIntegral))
      1 -> Pong <$> (get :: Get Word64)
      _ -> fail "Unrecognized packet!"


buildStatus :: String -> Int -> Int -> String -> StatusPayload
buildStatus version currentPlayers maxPlayers motd =
    StatusPayload (Version version 96)
                  (Players maxPlayers currentPlayers)
                  (Description motd)


data StatusPayload = StatusPayload
  { version       :: Version
  , players       :: Players
  , description   :: Description
  } deriving (Generic,Show,Eq,Read)


instance Aeson.ToJSON StatusPayload
instance Aeson.FromJSON StatusPayload


data Version = Version
  { name      :: String
  , protocol  :: Int
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Version
instance Aeson.FromJSON Version


data Players = Players
  { max     :: Int
  , online  :: Int
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Players
instance Aeson.FromJSON Players


data Description = Description
  { text    :: String
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Description
instance Aeson.FromJSON Description


parsePort :: B.ByteString -> Word16
parsePort b = toEnum (shiftL l 8 + r)
  where [l, r] = fmap fromEnum (B.unpack b)
