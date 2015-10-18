module OpenSandbox.Minecraft.Packet where


import qualified Data.Bytestring as B
import Data.Word


type VarInt = Int


data Handshake = Handshake
  { protoVersion      :: !VarInt
  , address           :: !B.Bytestring
  , port              :: !Word16
  , nextState         :: !VarInt
  } deriving (Show,Read,Eq)


data Request = Request deriving (Show,Eq,Read)
