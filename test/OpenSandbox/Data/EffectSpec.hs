module OpenSandbox.Data.EffectSpec (main,spec) where

import OpenSandbox.Data.Effect

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
