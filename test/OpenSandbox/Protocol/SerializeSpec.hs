module OpenSandbox.Protocol.SerializeSpec (main,spec) where

import Data.Serialize
import OpenSandbox.Protocol.Serialize
import Data.Either
import Data.Int
import Test.QuickCheck
import Test.Hspec

prop_varIntEq :: Int -> Bool
prop_varIntEq x =
  Right x == (runGet getVarInt (runPut (putVarInt x)) :: Either String Int)

prop_varLongEq :: Int64 -> Bool
prop_varLongEq x =
  Right x == (runGet getVarLong (runPut (putVarLong x)) :: Either String Int64)

spec :: Spec
spec = do
  describe "VarInt" $ do
    it "Identity" $ property prop_varIntEq
  describe "VarLong" $ do
    it "Identity" $ property prop_varLongEq

main :: IO ()
main = hspec spec
