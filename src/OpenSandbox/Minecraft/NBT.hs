-------------------------------------------------------------------------------
-- |
-- Copyright        : (c) 2015 Michael Carpenter
-- License          : GPL3
-- Maintainer       : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability        : provisional
-- Portability      : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.NBT (
    NBT
) where

-- External imports
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Int
import qualified Data.Text as T

data NBT = NBT Int NBTTagType NBTPayload deriving (Show,Eq,Read)

data NBTTagType = TAG_End
                | TAG_Byte
                | TAG_Short
                | TAG_Int
                | TAG_Long
                | TAG_Float
                | TAG_Double
                | TAG_Byte_Array
                | TAG_String
                | TAG_List
                | TAG_Compound
                | TAG_Int_Array
                deriving (Show,Eq,Read,Enum)

data NBTPayload = TagEnd
                | TagByte Int8
                | TagShort Int16
                | TagInt Int32
                | TagLong Int64
                | TagFloat Float
                | TagDouble Double
                | TagByteArray B.ByteString
                | TagString T.Text
                | TagList [NBTPayload]
                | TagCompound [NBT]
                deriving (Show,Eq,Read)
