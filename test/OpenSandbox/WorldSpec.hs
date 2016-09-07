{-# LANGUAGE FlexibleInstances #-}

module OpenSandbox.WorldSpec (main,spec) where

import Control.Monad
import Data.Either
import Data.Serialize
import qualified Data.Vector as V
import Data.Word
import Test.Hspec
import Test.QuickCheck
import OpenSandbox.Data.BlockSpec()
import OpenSandbox.Data.Block (BlockStateID,BlockIndice)
import OpenSandbox.World

instance Arbitrary BlockIndice where
  arbitrary = (fromIntegral <$> (arbitrary :: Gen Word8) :: Gen BlockIndice)

instance Arbitrary BlockIndices where
  arbitrary = BlockIndices <$> vectorOf 4096 arbitrary

instance Arbitrary OChunkColumnData where
  arbitrary = OChunkColumnData <$> vectorOf 16 arbitrary

instance Arbitrary DChunkColumnData where
  arbitrary = DChunkColumnData <$> vectorOf 8 arbitrary

instance Arbitrary OChunkBlock where
  arbitrary = OChunkBlock
              <$> arbitrary
              <*> (V.fromList <$> vectorOf 2048 arbitrary)
              <*> (V.fromList <$> vectorOf 2048 arbitrary)

instance Arbitrary DChunkBlock where
  arbitrary = DChunkBlock
              <$> arbitrary
              <*> (V.fromList <$> vectorOf 2048 arbitrary)

instance Arbitrary ChunkBlockData where
  arbitrary = (ChunkBlockData . V.fromList) <$> vectorOf 4096 arbitrary

instance Arbitrary BiomeIndices where
  arbitrary = (BiomeIndices . V.fromList) <$> vectorOf 256 arbitrary

instance Arbitrary BitsPerBlock where
  arbitrary = fmap mkBitsPerBlock (arbitrary :: Gen BitsPerBlockOption)

instance Arbitrary BitsPerBlockOption where
  arbitrary = fmap toEnum (choose (4,13) :: Gen Int)

prop_IdentityOChunkBlock :: OChunkBlock -> Bool
prop_IdentityOChunkBlock chunk =
  Right chunk == (decode (encode chunk) :: Either String OChunkBlock)

prop_IdentityDChunkBlock :: DChunkBlock -> Bool
prop_IdentityDChunkBlock chunk =
  Right chunk == (decode (encode chunk) :: Either String DChunkBlock)

prop_IdentityBiomeIndices :: BiomeIndices -> Bool
prop_IdentityBiomeIndices biomeIndices =
  Right biomeIndices == (decode (encode biomeIndices) :: Either String BiomeIndices)

prop_IdentityBitsPerBlock :: BitsPerBlock -> Bool
prop_IdentityBitsPerBlock bpb =
  Right bpb == (decode (encode bpb) :: Either String BitsPerBlock)

prop_IdentityPackIndices :: BlockIndices -> Bool
prop_IdentityPackIndices indices =
  unBlockIndices indices == unpackIndices bpb 0 0 (packIndices bpb 0 0 indices)
  where
    bpb = mkBitsPerBlock BitsPerBlock16

prop_IdentityCompressIndices :: ChunkBlockData -> Bool
prop_IdentityCompressIndices indices = indices == decompressIndices (compressIndices indices)

spec :: Spec
spec = return ()
{-
spec = do
  describe "OChunkColumnData" $ do
    it "Identity" $ property prop_IdentityOChunkBlock
  describe "DChunkColumnData" $ do
    it "Identity" $ property prop_IdentityDChunkBlock
  describe "OChunkBlock" $ do
    it "Identity" $ property prop_IdentityOChunkBlock
  describe "DChunkBlock" $ do
    it "Identity" $ property prop_IdentityDChunkBlock
  describe "BiomeIndices" $ do
    it "Identity" $ property prop_IdentityBiomeIndices
  describe "BitsPerBlock" $ do
    it "Identity" $ property prop_IdentityBitsPerBlock
  describe "Packing Indices" $ do
    it "Identity" $ property prop_IdentityPackIndices
  describe "Compressing Indices" $ do
    it "Identity" $ property prop_IdentityCompressIndices
-}
main :: IO ()
main = hspec spec
