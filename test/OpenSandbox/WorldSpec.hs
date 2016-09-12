{-# LANGUAGE FlexibleInstances #-}

module OpenSandbox.WorldSpec (main,spec) where

import Control.Monad
import Data.NBT
import Data.Serialize
import qualified Data.Vector as V
import Data.Word
import Test.Hspec
import Test.QuickCheck
import OpenSandbox.Data.BlockSpec()
import OpenSandbox.Data.Block (BlockStateID,BlockIndice)
import OpenSandbox.World
import Common
import Data.NBTSpec()

instance Arbitrary BlockIndice where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word8) :: Gen BlockIndice

instance Arbitrary BlockIndices where
  arbitrary = BlockIndices <$> vectorOf 4096 arbitrary

instance Arbitrary ChunkColumn where
  arbitrary =
    ChunkColumn
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ChunkColumnData where
  arbitrary = do
    k <- choose (1,16) :: Gen Int
    ChunkColumnData <$> vectorOf k arbitrary

instance Arbitrary ChunkBlock where
  arbitrary = ChunkBlock
              <$> arbitrary
              <*> (V.fromList <$> vectorOf 2048 arbitrary)
              <*> (V.fromList <$> vectorOf 2048 arbitrary)

instance Arbitrary ChunkBlockData where
  arbitrary = (ChunkBlockData . V.fromList) <$> vectorOf 4096 arbitrary

instance Arbitrary BiomeIndices where
  arbitrary = (BiomeIndices . V.fromList) <$> vectorOf 256 arbitrary

instance Arbitrary BitsPerBlock where
  arbitrary = fmap mkBitsPerBlock (arbitrary :: Gen BitsPerBlockOption)

instance Arbitrary BitsPerBlockOption where
  arbitrary = fmap toEnum (choose (4,13) :: Gen Int)

instance Arbitrary PrimaryBitMask where
  arbitrary = mkPrimaryBitMask <$> arbitrary

prop_IdentityPackIndices :: BlockIndices -> Bool
prop_IdentityPackIndices indices =
  unBlockIndices indices == unpackIndices bpb 0 0 (packIndices bpb 0 0 indices)
  where
    bpb = mkBitsPerBlock BitsPerBlock16

prop_IdentityCompressIndices :: ChunkBlockData -> Bool
prop_IdentityCompressIndices indices = indices == decompressIndices (compressIndices indices)

spec :: Spec
spec = do
  describe "ChunkColumn" $ do
    it "Identity" $ quickCheckWith
      stdArgs { maxSize = 20, maxSuccess = 20 }
      (prop_SerializeIdentity :: ChunkColumn -> Bool)
  describe "ChunkColumnData" $ do
    it "Identity" $ quickCheckWith
      stdArgs { maxSize = 20, maxSuccess = 20 }
      (prop_SerializeIdentity :: ChunkColumnData -> Bool)
  describe "ChunkBlock" $ do
    it "Identity" $ quickCheckWith
      stdArgs { maxSize = 20, maxSuccess = 50 }
      (prop_SerializeIdentity :: ChunkBlock -> Bool)
  describe "BiomeIndices" $ do
    it "Identity" $ quickCheckWith
      stdArgs
      (prop_SerializeIdentity :: BiomeIndices -> Bool)
  describe "BitsPerBlock" $ do
    it "Identity" $ quickCheckWith
      stdArgs
      (prop_SerializeIdentity :: BitsPerBlock -> Bool)
  describe "PrimaryBitMask" $ do
    it "Identity" $ quickCheckWith
      stdArgs
      (prop_SerializeIdentity :: PrimaryBitMask -> Bool)
  describe "Packing Indices" $ do
    it "Identity" $ quickCheckWith
      stdArgs
      prop_IdentityPackIndices
  describe "Compressing Indices" $ do
    it "Identity" $ quickCheckWith
      stdArgs
      prop_IdentityCompressIndices

main :: IO ()
main = hspec spec
