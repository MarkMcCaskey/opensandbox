{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module OpenSandbox.World.FlatSpec (main,spec) where

import qualified Data.Vector as V
import Test.Hspec
import Test.QuickCheck

import OpenSandbox.Data.BlockSpec()
import OpenSandbox.World.Flat

instance Arbitrary ChunkLayers where
  arbitrary = do
    n <- choose (0,256)
    eitherLayers <- mkChunkLayers . V.fromList <$> vectorOf n arbitrary
    case eitherLayers of
      Left _ -> undefined
      Right layers -> return layers

spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
