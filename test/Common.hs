module Common
  ( prop_SerializeIdentity
  , prop_CustomSerializeIdentity
  ) where

import qualified Data.ByteString as B
import Data.Serialize
import qualified Data.Text as T
import Test.QuickCheck

instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

prop_SerializeIdentity :: (Serialize a, Eq a) => a -> Bool
prop_SerializeIdentity x = Right x == decode (encode x)

prop_CustomSerializeIdentity :: (Eq a) => Putter a -> Get a -> a -> Bool
prop_CustomSerializeIdentity p g x = Right x == runGet g (runPut (p x))
