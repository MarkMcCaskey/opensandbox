module CommonSpec (main,spec) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Test.QuickCheck
import Test.Hspec

instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
