module OpenSandbox.Minecraft.Protocol.Types
  ( VarInt
  , VarLong
  , Slot
  , Position
  ) where


import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Int
import Data.Word
import Linear.V3


-- Type Conversion Legend
-- Boolean        -> Bool
-- Byte           -> Int8
-- Unsigned Byte  -> Word8
-- Short          -> Int16
-- Unsigned Short -> Word16
-- Int            -> Int32
-- Long           -> Int64
-- Float          -> Float
-- Double         -> Double
-- String         -> Text
-- Chat           -> Text
-- VarInt         -> Int (1 <= VarInt <= 5 bytes)
-- VarLong        -> Long (1 <= VarLong <= 10 bytes)


type VarInt = Int
type VarLong = Int64


type Slot = B.ByteString
type Position = V3 Int32
