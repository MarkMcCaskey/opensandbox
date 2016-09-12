{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module OpenSandbox.Data.BlockSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Word

import OpenSandbox.Data.Block

instance Arbitrary BlockStateID where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word64)

spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
