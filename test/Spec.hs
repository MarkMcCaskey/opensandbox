import qualified  Data.ByteString as B
import            Data.Either
import            Data.Maybe
import            Data.Serialize
import qualified  Data.Set as Set
import            OpenSandbox
import            Test.Hspec
import            Test.QuickCheck
import            Control.Exception (evaluate)
import            System.Directory

testSrvDir :: FilePath
testSrvDir = "test/testserver"

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

main :: IO ()
main = hspec $ do
    describe "OpenSandbox.Tmux.parseTmuxID" $ do
      it "should build a TmuxID from a String ++ ':' ++ String" $ do
        isJust (parseTmuxID "opensandbox:25565") `shouldBe` True
      it "should return Nothing if it is given a string that has more or less than 1 ':'" $ do
        parseTmuxID "opensandbox25565" `shouldBe` Nothing
        parseTmuxID "opensandbox::25565" `shouldBe` Nothing
      it "should return Nothing if either the sessionID or the windowID are an empty string" $ do
        parseTmuxID ":25565" `shouldBe` Nothing
        parseTmuxID "opensandbox:" `shouldBe` Nothing
