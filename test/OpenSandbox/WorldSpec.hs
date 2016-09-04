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
import OpenSandbox.World

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

prop_IdentityOverWorldChunkBlock :: [OverWorldChunkBlock] -> Bool
prop_IdentityOverWorldChunkBlock [] = True
prop_IdentityOverWorldChunkBlock lst = do
  let encoded = encode lst
  let decoded = decode encoded :: Either String [OverWorldChunkBlock]
  case decoded of
    Left _ -> False
    Right lst' -> lst == lst'

prop_IdentityOtherWorldChunkBlock :: [OtherWorldChunkBlock] -> Bool
prop_IdentityOtherWorldChunkBlock [] = True
prop_IdentityOtherWorldChunkBlock lst = do
  let encoded = encode lst
  let decoded = decode encoded :: Either String [OtherWorldChunkBlock]
  case decoded of
    Left _ -> False
    Right lst' -> lst == lst'

prop_IdentityChunkBlockData :: [ChunkBlockData] -> Bool
prop_IdentityChunkBlockData [] = True
prop_IdentityChunkBlockData lst = do
  let encoded = encode lst
  let decoded = decode encoded :: Either String [ChunkBlockData]
  case decoded of
    Left _ -> False
    Right lst' -> lst == lst'

prop_IdentityBiomeIndices :: [BiomeIndices] -> Bool
prop_IdentityBiomeIndices [] = True
prop_IdentityBiomeIndices lst = do
  let encoded = encode lst
  let decoded = decode encoded :: Either String [BiomeIndices]
  case decoded of
    Left _ -> False
    Right lst' -> lst == lst'

prop_IdentityBitsPerBlock :: [BitsPerBlock] -> Bool
prop_IdentityBitsPerBlock [] = True
prop_IdentityBitsPerBlock lst = do
  let encoded = encode lst
  let decoded = decode encoded :: Either String [BitsPerBlock]
  case decoded of
    Left _ -> False
    Right lst' -> lst == lst'

prop_IdentityIndices :: ChunkBlockData -> Bool
--prop_IdentityIndices [] = True
prop_IdentityIndices x = (traceShowId x) == (decodeIndices . encodeIndices $ x)

spec :: Spec
spec = do
--  describe "OverWorldChunkBlock" $ do
--    it "Identity" $ property prop_IdentityOverWorldChunkBlock
--  describe "OtherWorldChunkBlock" $ do
--    it "Identity" $ property prop_IdentityOtherWorldChunkBlock
  describe "ChunkBlockData" $ do
    it "Identity" $ property prop_IdentityChunkBlockData
  describe "BiomeIndices" $ do
    it "Identity" $ property prop_IdentityBiomeIndices
  describe "BitsPerBlock" $ do
    it "Identity" $ property prop_IdentityBitsPerBlock
  describe "Encoding Indices" $ do
    it "Identity" $ verboseCheck prop_IdentityIndices

main :: IO ()
main = hspec spec
