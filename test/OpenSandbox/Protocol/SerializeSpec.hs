module OpenSandbox.Protocol.SerializeSpec (main,spec) where

import Data.Serialize
import OpenSandbox.Protocol.Serialize
import Data.Either
import Data.Int
import Test.QuickCheck
import Test.Hspec

prop_varIntEq :: [Int] -> Bool
prop_varIntEq [] = True
prop_varIntEq lst = do
  let encoded = fmap (\x -> runPut (putVarInt x)) lst
  let decoded = fmap (runGet getVarInt) encoded :: [Either String Int]
  lst == rights decoded

prop_varLongEq :: [Int64] -> Bool
prop_varLongEq [] = True
prop_varLongEq lst = do
  let encoded = fmap (\x -> runPut (putVarLong x)) lst
  let decoded = fmap (runGet getVarLong) encoded :: [Either String Int64]
  lst == rights decoded

spec :: Spec
spec = do
  describe "VarInt" $ do
    it "Identity" $ property prop_varIntEq
  describe "VarLong" $ do
    it "Identity" $ property prop_varLongEq

main :: IO ()
main = hspec spec
