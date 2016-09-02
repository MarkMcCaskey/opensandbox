{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.NBTSpec (main,spec) where
import qualified  Data.Array.IArray as A
import            Data.Array.Unboxed (listArray)
import            Data.Int
import            Data.NBT
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID
import            Data.NBT
import            Data.NBT.Decode
import            Data.NBT.Encode
import            Data.NBT.Types
import qualified  Data.Vector as V
import qualified  Data.Vector.Unboxed as U
import            Data.Word
import            Test.Hspec
import            Test.QuickCheck

import CommonSpec()

instance Arbitrary (A.Array Int32 Int8) where
  arbitrary = do
    ln <- choose (0,10) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ A.array (0,(ln-1)) (zip (A.range (0,(ln-1))) e)

instance Arbitrary (U.Vector Int32) where
  arbitrary = do
    ln <- choose (0,10) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ U.fromList e

instance Arbitrary (U.Vector Int8) where
  arbitrary = do
    ln <- choose (0,10) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ U.fromList e

instance Arbitrary NBT where
  arbitrary = sized nbt'
    where nbt' 0 = return $ TagByte "" 5
          nbt' n | n > 0 = do
            tagID <- choose (0x01,0x0b) :: Gen Word8

            case tagID of
              0x01 -> TagByte <$> arbitrary <*> arbitrary
              0x02 -> TagShort <$> arbitrary <*> arbitrary
              0x03 -> TagInt <$> arbitrary <*> arbitrary
              0x04 -> TagLong <$> arbitrary <*> arbitrary
              0x05 -> TagFloat <$> arbitrary <*> arbitrary
              0x06 -> TagDouble <$> arbitrary <*> arbitrary
              0x07 -> TagByteArray <$> arbitrary <*> arbitrary
              0x08 -> TagString <$> arbitrary <*> arbitrary
              0x09 -> TagList <$> arbitrary <*> arbitrary
              0x0a -> TagCompound <$> arbitrary <*> vectorOf (n `div` 30) arbitrary
              0x0b -> TagIntArray <$> arbitrary <*> arbitrary

instance Arbitrary NBTList where
  arbitrary = sized nbtlst'
    where nbtlst' 0 = return $ NBTList TypeByte [NTagByte 0]
          nbtlst' n | n > 0 = do
            tagID <- choose (0x01,0x0b) :: Gen Word8
            i <- choose (0,5) :: Gen Int
            case tagID of
              0x01 -> NBTList <$> return TypeByte <*> vectorOf i (NTagByte <$> (arbitrary :: Gen Int8))
              0x02 -> NBTList <$> return TypeShort <*> vectorOf i (NTagShort <$> (arbitrary :: Gen Int16))
              0x03 -> NBTList <$> return TypeInt <*> vectorOf i (NTagInt <$> (arbitrary :: Gen Int32))
              0x04 -> NBTList <$> return TypeLong <*> vectorOf i (NTagLong <$> (arbitrary :: Gen Int64))
              0x05 -> NBTList <$> return TypeFloat <*> vectorOf i (NTagFloat <$> (arbitrary :: Gen Float))
              0x06 -> NBTList <$> return TypeDouble <*> vectorOf i (NTagDouble <$> (arbitrary :: Gen Double))
              0x07 -> NBTList <$> return TypeByteArray <*> vectorOf (n `div` 20) (NTagByteArray <$> (arbitrary :: Gen (U.Vector Int8)))
              0x08 -> NBTList <$> return TypeString <*> vectorOf i (NTagString <$> (arbitrary :: Gen T.Text))
              0x09 -> NBTList <$> return TypeList <*> vectorOf (n `div` 20) (NTagList <$> arbitrary)
              0x0a -> NBTList <$> return TypeCompound <*> vectorOf (n `div` 30) (NTagCompound <$> vectorOf (n `div` 30) (arbitrary :: Gen NBT))
              0x0b -> NBTList <$> return TypeIntArray <*> vectorOf (n `div` 20) (NTagIntArray <$> (arbitrary :: Gen (U.Vector Int32)))

spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
