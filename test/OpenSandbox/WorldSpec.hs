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
import Debug.Trace
import OpenSandbox.Data.Block (BlockStateID,BlockIndice)
import OpenSandbox.World

instance Arbitrary BlockIndice where
  arbitrary = (fromIntegral <$> (arbitrary :: Gen Word8) :: Gen BlockIndice)

instance Arbitrary BlockIndices where
  arbitrary = BlockIndices <$> vectorOf 4096 arbitrary

instance Arbitrary OverWorldChunkBlock where
  arbitrary = OverWorldChunkBlock
              <$> arbitrary
              <*> (V.fromList <$> vectorOf 2048 arbitrary)
              <*> (V.fromList <$> vectorOf 2048 arbitrary)

instance Arbitrary OtherWorldChunkBlock where
  arbitrary = OtherWorldChunkBlock
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

prop_IdentityOverWorldChunkBlock :: OverWorldChunkBlock -> Bool
prop_IdentityOverWorldChunkBlock chunk =
  Right chunk == (decode (encode chunk) :: Either String OverWorldChunkBlock)

prop_IdentityOtherWorldChunkBlock :: OtherWorldChunkBlock -> Bool
prop_IdentityOtherWorldChunkBlock chunk =
  Right chunk == (decode (encode chunk) :: Either String OtherWorldChunkBlock)

prop_IdentityBiomeIndices :: BiomeIndices -> Bool
prop_IdentityBiomeIndices biomeIndices =
  Right biomeIndices == (decode (encode biomeIndices) :: Either String BiomeIndices)

prop_IdentityBitsPerBlock :: BitsPerBlock -> Bool
prop_IdentityBitsPerBlock bpb =
  Right bpb == (decode (encode bpb) :: Either String BitsPerBlock)

prop_IdentityPackIndices :: BlockIndices -> Bool
prop_IdentityPackIndices indices = all (==unBlockIndices indices)
  [ unpackIndices bpbI 0 0 (packIndices bpbI 0 0 indices)
  , unpackIndices bpbII 0 0 (packIndices bpbII 0 0 indices)
  , unpackIndices bpbIII 0 0 (packIndices bpbIII 0 0 indices)
  ]
  where
    bpbI = mkBitsPerBlock BitsPerBlock8
    bpbII = mkBitsPerBlock BitsPerBlock13
    bpbIII = mkBitsPerBlock BitsPerBlock16

spec :: Spec
spec = do
  --describe "OverWorldChunkBlock" $ do
  --  it "Identity" $ property prop_IdentityOverWorldChunkBlock
  --describe "OtherWorldChunkBlock" $ do
  --  it "Identity" $ property prop_IdentityOtherWorldChunkBlock
  describe "BiomeIndices" $ do
    it "Identity" $ property prop_IdentityBiomeIndices
  describe "BitsPerBlock" $ do
    it "Identity" $ property prop_IdentityBitsPerBlock
  describe "Packing Indices" $ do
    it "Identity" $ property prop_IdentityPackIndices

main :: IO ()
main = hspec spec
