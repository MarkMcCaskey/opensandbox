{-# LANGUAGE FlexibleInstances #-}
module OpenSandbox.WorldSpec (main,spec) where

import qualified Data.Vector as V
import Test.Hspec
import Test.QuickCheck
import OpenSandbox.Data.BlockSpec()

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

spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
