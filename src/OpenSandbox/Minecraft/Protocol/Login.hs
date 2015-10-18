module OpenSandbox.Minecraft.Protocol.Login
  ( Disconnect (..)
  , EncryptionRequest (..)
  , LoginSuccess (..)
  , SetCompression (..)
  , LoginStart (..)
  , EncryptionResponse (..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T

import OpenSandbox.Minecraft.Protocol.Types

-------------------------------------------------------------------------------
-- Clientbound
-------------------------------------------------------------------------------

-- Packet ID: 0x00
data Disconnect = Disconnect
  { reason :: !T.Text
  } deriving (Show,Eq,Read)

-- Packet ID: 0x01
data EncryptionRequest = EncryptionRequest
  { serverID              :: !T.Text
  , publicKeyLength       :: !VarInt
  , publicKey             :: !B.ByteString
  , verifyTokenLengthReq  :: !VarInt
  , verifyTokenReq        :: !B.ByteString
  } deriving (Show,Eq,Read)

-- Packet ID: 0x02
data LoginSuccess = LoginSuccess
  { uuid      :: !T.Text
  , username  :: !T.Text
  } deriving (Show,Eq,Read)

-- Packet ID: 0x03
data SetCompression = SetCompession
  { threshold :: !VarInt
  } deriving (Show,Eq,Read)

-------------------------------------------------------------------------------
-- Serverbound
-------------------------------------------------------------------------------

-- Packet ID: 0x00
data LoginStart = LoginStart
  { name :: !T.Text
  } deriving (Show,Eq,Read)

-- Packet ID: 0x01
data EncryptionResponse = EncryptionResponse
  { sharedSecretLength    :: !VarInt
  , sharedSecret          :: !B.ByteString
  , verifyTokenLengthResp :: !VarInt
  , verifyTokenResp       :: !B.ByteString
  } deriving (Show,Eq,Read)
