{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common
  ( prop_SerializeIdentity
  , prop_CustomSerializeIdentity
  ) where

import qualified Data.ByteString as B
import Data.Serialize
import qualified Data.Text as T
import Data.UUID
import qualified Data.Vector as V
import Test.QuickCheck

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList $ listOf arbitrary

instance Arbitrary UUID where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ fromWords a b c d

instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

prop_SerializeIdentity :: (Serialize a, Eq a) => a -> Bool
prop_SerializeIdentity x = Right x == decode (encode x)

prop_CustomSerializeIdentity :: (Eq a) => Putter a -> Get a -> a -> Bool
prop_CustomSerializeIdentity p g x = Right x == runGet g (runPut (p x))
