-------------------------------------------------------------------------------
-- |
-- Module           : OpenSandbox.NBT
-- Copyright        : (c) 2015 Michael Carpenter
-- License          : GPL3
-- Maintainer       : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability        : experimental
-- Portability      : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.NBT (NBT) where


import            Data.Binary
import qualified  Data.ByteString.Lazy as B
import            Data.Int
import qualified  Data.Text as T


data NBT = NBT Int NBTTagType NBTPayload deriving (Show,Eq,Read)


data NBTTagType
  = TAGEnd
  | TAGByte
  | TAGShort
  | TAGInt
  | TAGLong
  | TAGFloat
  | TAGDouble
  | TAGByteArray
  | TAGString
  | TAGList
  | TAGCompound
  | TAGIntArray
  deriving (Show,Eq,Read,Enum)


data NBTPayload
  = TagEnd
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
