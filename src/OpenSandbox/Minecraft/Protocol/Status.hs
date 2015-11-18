{-# LANGUAGE DeriveGeneric #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.Protocol.Status
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Protocol.Status
    ( ClientBoundStatus (..)
    , ServerBoundStatus (..)
    , ResponsePayload
    , Version
    , Players
    , Description
    , buildResponse
    ) where


import qualified  Data.Aeson as Aeson
import qualified  Data.ByteString.Char8 as B
import qualified  Data.Text as T
import            Data.Binary
import            Data.Binary.Get
import            Data.Binary.Put
import            Data.Bits
import            GHC.Generics


data ServerBoundStatus
    = Handshake Word8 B.ByteString Word16 Word8
    | Request
    | Ping Word64
    deriving (Show,Eq,Read)


data ClientBoundStatus
    = Response B.ByteString
    | Pong Word64
    deriving (Show,Eq,Read)


instance Binary ServerBoundStatus where
  put (Handshake v a p s) = do
    put (fromIntegral $ 6 + B.length a :: Word8)
    put (0 :: Word8)
    put v
    put (fromIntegral $ B.length a :: Word8)
    putByteString a
    put p
    put s
  put (Ping payload) = do
    put (fromIntegral $ 2 + 8 :: Word8)
    put (1 :: Word8)
    putWord64be payload

  get = do
    _ <- getWord8
    packetID <- getWord8
    case packetID of
      0 -> Handshake <$> getWord8 <*> (getWord8 >>= (\x -> getByteString (fromIntegral x))) <*> getWord16be <*> getWord8
      1 -> Ping <$> (get :: Get Word64)
      _ -> fail "Unrecognized packet!"


instance Binary ClientBoundStatus where
  put (Response payload) = do
    put (fromIntegral $ 2 + B.length payload :: Word8)
    put (0 :: Word8)
    put (fromIntegral $ B.length payload :: Word8)
    putByteString payload

  get = do
    _ <- getWord8
    packetID <- getWord8
    case packetID of
      0 -> Response <$> (getWord8 >>= (\x -> getByteString (fromIntegral x)))
      1 -> Pong <$> (get :: Get Word64)
      _ -> fail "Unrecognized packet!"


buildResponse :: T.Text -> Int -> Int -> T.Text -> ResponsePayload
buildResponse version currentPlayers maxPlayers motd =
    ResponsePayload (Version version 85)
                    (Players maxPlayers currentPlayers)
                    (Description motd)


data ResponsePayload = ResponsePayload
  { version       :: Version
  , players       :: Players
  , description   :: Description
  } deriving (Generic,Show,Eq,Read)


instance Aeson.ToJSON ResponsePayload
instance Aeson.FromJSON ResponsePayload


data Version = Version
  { name      :: T.Text
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
  { text    :: T.Text
  } deriving (Generic,Eq,Show,Read)


instance Aeson.ToJSON Description
instance Aeson.FromJSON Description


parsePort :: B.ByteString -> Word16
parsePort b = toEnum ((shiftL l 8) + r)
  where (l:r:[]) = fmap fromEnum (B.unpack b)
