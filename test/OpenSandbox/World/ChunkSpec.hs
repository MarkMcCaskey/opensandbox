{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module OpenSandbox.World.ChunkSpec (main,spec) where

import Common
import Control.Monad
import Data.NBTSpec()
import qualified Data.Vector as V
import Data.Word
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Test
import OpenSandbox.Data.Block (BlockIndice)
import OpenSandbox.Data.BlockSpec()
import OpenSandbox.World

instance Arbitrary BlockIndice where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word8) :: Gen BlockIndice

instance Arbitrary BlockIndices where
  arbitrary = BlockIndices <$> vectorOf 4096 arbitrary

instance Arbitrary ChunkColumn where
  arbitrary = mkChunkColumn <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ChunkColumnData where
  arbitrary = do
    k <- choose (1,16) :: Gen Int
    chunks <- V.fromList <$> vectorOf k arbitrary
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
  describe "ChunkColumn" $
    it "Identity" $ do
      r <- quickCheckWithResult
        stdArgs { maxSize = 20, maxSuccess = 20 }
        (prop_SerializeIdentity :: ChunkColumn -> Bool)
      unless (isSuccess r) $ print r
      isSuccess r `shouldBe` True
  describe "ChunkColumnData" $
    it "Identity" $ do
      r <- quickCheckWithResult
        stdArgs { maxSize = 20, maxSuccess = 20 }
        (prop_SerializeIdentity :: ChunkColumnData -> Bool)
      unless (isSuccess r) $ print r
      isSuccess r `shouldBe` True
  describe "ChunkBlock" $
    it "Identity" $ do
      r <- quickCheckWithResult
        stdArgs { maxSize = 20, maxSuccess = 50 }
        (prop_SerializeIdentity :: ChunkBlock -> Bool)
      unless (isSuccess r) $ print r
      isSuccess r `shouldBe` True
  describe "BiomeIndices" $
    it "Identity" $ do
      r <- quickCheckWithResult
        stdArgs
        (prop_SerializeIdentity :: BiomeIndices -> Bool)
      unless (isSuccess r) $ print r
      isSuccess r `shouldBe` True
  describe "BitsPerBlock" $
    it "Identity" $ do
      r <- quickCheckWithResult
        stdArgs
        (prop_SerializeIdentity :: BitsPerBlock -> Bool)
      unless (isSuccess r) $ print r
      isSuccess r `shouldBe` True
  describe "PrimaryBitMask" $
    it "Identity" $ do
      r <- quickCheckWithResult
        stdArgs
        (prop_SerializeIdentity :: PrimaryBitMask -> Bool)
      unless (isSuccess r) $ print r
      isSuccess r `shouldBe` True
  describe "Packing Indices" $
    it "Identity" $ do
      r <- quickCheckWithResult
        stdArgs
        prop_IdentityPackIndices
      unless (isSuccess r) $ print r
      isSuccess r `shouldBe` True
  describe "Compressing Indices" $
    it "Identity" $ do
      r <- quickCheckWithResult
        stdArgs
        prop_IdentityCompressIndices
      unless (isSuccess r) $ print r
      isSuccess r `shouldBe` True

main :: IO ()
main = hspec spec
