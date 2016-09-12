{-# LANGUAGE FlexibleInstances #-}

module OpenSandbox.WorldSpec (main,spec) where

import qualified Data.Vector as V
import Data.Word
import Test.Hspec
import Test.QuickCheck
import OpenSandbox.Data.BlockSpec()
import OpenSandbox.Data.Block (BlockIndice)
import OpenSandbox.World
import Common
import Data.NBTSpec()

instance Arbitrary BlockIndice where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word8) :: Gen BlockIndice

instance Arbitrary BlockIndices where
  arbitrary = BlockIndices <$> vectorOf 4096 arbitrary

instance Arbitrary ChunkColumn where
  arbitrary = mkChunkColumn <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ChunkColumnData where
  arbitrary = do
    k <- choose (1,16) :: Gen Int
    chunks <- vectorOf k arbitrary
    case mkChunkColumnData chunks of
      Left err -> fail err
      Right chunkColumnData -> return chunkColumnData

instance Arbitrary ChunkBlock where
  arbitrary = do
    dat <- arbitrary :: Gen ChunkBlockData
    blockLight <- (V.fromList <$> vectorOf 2048 arbitrary)
    skyLight <- (V.fromList <$> vectorOf 2048 arbitrary)
    case mkChunkBlock dat blockLight skyLight of
      Left err -> fail err
      Right chunkBlock -> return chunkBlock

instance Arbitrary ChunkBlockData where
  arbitrary = do
    blocks <- vectorOf 4096 arbitrary
    case (mkChunkBlockData . V.fromList $ blocks) of
      Left err -> fail err
      Right chunkBlockData -> return chunkBlockData

instance Arbitrary BiomeIndices where
  arbitrary = do
    indices <- vectorOf 256 arbitrary
    case (mkBiomeIndices . V.fromList $ indices) of
      Left err -> fail err
      Right biomeIndices -> return biomeIndices

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
prop_IdentityCompressIndices indices = indices == decompressIndices bpb palette (compressIndices bpb palette indices)
  where
    (bpb,palette) = calcPalette indices

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
