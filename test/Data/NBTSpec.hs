{-# LANGUAGE OverloadedStrings #-}
module Data.NBTSpec (main,spec) where

import            Control.Monad
import qualified  Data.Array.IArray as IA
import            Data.Array.Unboxed (listArray)
import            Data.NBT
import qualified  Data.Text as T
import            Test.Hspec
import            Test.QuickCheck

instance Arbitrary TagType where
    arbitrary = toEnum <$> choose (1, 11)

instance Arbitrary NBT where
  arbitrary = arbitrary >>= \(ty, nm) -> NBT (T.pack nm) <$> mkArb ty
    where
      mkArb ty =
        case ty of
          EndType    -> error "can't make end-type"
          ByteType   -> ByteTag   <$> arbitrary
          ShortType  -> ShortTag  <$> arbitrary
          IntType    -> IntTag    <$> arbitrary
          LongType   -> LongTag   <$> arbitrary
          FloatType  -> FloatTag  <$> arbitrary
          DoubleType -> DoubleTag <$> arbitrary
          ByteArrayType -> do
            len  <- choose (0, 100)
            ws   <- replicateM len arbitrary
            let a = listArray (0, fromIntegral len - 1) ws
            return (ByteArrayTag a)
          StringType -> do
            len <- choose (0, 100)
            str <- T.pack <$> replicateM len arbitrary
            return (StringTag str)
          ListType -> do
            len   <- choose (0, 10) -- list types nest, don't get too big
            subTy <- arbitrary
            ts    <- replicateM len (mkArb subTy)
            let a  = IA.listArray (0, fromIntegral len - 1) ts
            return (ListTag a)
          CompoundType -> do
            len <- choose (0, 10) -- compound types nest, don't get too big
            ts  <- replicateM len arbitrary
            return (CompoundTag ts)
          IntArrayType -> do
            len  <- choose (0, 100)
            v    <- vector len
            let a = listArray (0, fromIntegral len - 1) v
            return (IntArrayTag a)

spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
