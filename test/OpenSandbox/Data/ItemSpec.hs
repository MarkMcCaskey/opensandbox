module OpenSandbox.Data.ItemSpec (main,spec) where

import OpenSandbox.Data.Item

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
