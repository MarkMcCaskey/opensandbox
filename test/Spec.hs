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

userCachePath :: FilePath
userCachePath = testSrvDir ++ "/" ++ "usercache.json"

userCacheMangledPath :: FilePath
userCacheMangledPath = testSrvDir ++ "/" ++ "mangledusercache.json"

userCacheSingletonPath :: FilePath
userCacheSingletonPath = testSrvDir ++ "/" ++ "singletonusercache.json"

userCacheEmptyPath :: FilePath
userCacheEmptyPath = testSrvDir ++ "/" ++ "emptyusercache.json"

opsPath :: FilePath
opsPath = testSrvDir ++ "/" ++ "ops.json"

whiteListPath :: FilePath
whiteListPath = testSrvDir ++ "/" ++ "whitelist.json"

bannedIPsPath :: FilePath
bannedIPsPath = testSrvDir ++ "/" ++ "banned-ips.json"

bannedPlayersPath :: FilePath
bannedPlayersPath = testSrvDir ++ "/" ++ "banned-players.json"


instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary


main :: IO ()
main = hspec $ do
    describe "OpenSandbox.Minecraft.User.readUserCache" $ do
      it "returns a list of users from correct usercache.json" $ do
        shouldBeUsers <- readUserCache userCachePath
        isRight shouldBeUsers `shouldBe` True
      it "returns only errors when not reading usercache.json" $ do
        shouldBeOps <- readUserCache opsPath
        shouldBeWhiteList <- readUserCache whiteListPath
        shouldBeBannedIPs <- readUserCache bannedIPsPath
        shouldBeBannedPlayers <- readUserCache bannedPlayersPath
        rights [shouldBeOps,shouldBeWhiteList,shouldBeBannedIPs,shouldBeBannedPlayers] `shouldBe` []
      it "returns an error when reading a mangled usercache.json" $ do
        shouldBeError <- readUserCache userCacheMangledPath
        shouldBeError `shouldBe` (Left "Error >> Failed reading: satisfy")
      it "returns a singleton list of users when reading a usercache.json with one entry" $ do
        shouldBeOneUser <- readUserCache userCacheSingletonPath
        (length $ head $ rights [shouldBeOneUser]) `shouldBe` (1 :: Int)
      it "returns an empty list of users when reading empty usercache.json" $ do
        shouldBeNoUsers <-readUserCache userCacheEmptyPath
        (null $ head $ rights [shouldBeNoUsers]) `shouldBe` True

    describe "OpenSandbox.Minecraft.User.createUserGroup" $ do
      it "should return a set of the same size as usercache.json" $ do
        shouldBeUsers <- readUserCache userCachePath
        let lst = (head $ rights [shouldBeUsers])
        let shouldBeUGroup = createUserGroup lst
        Set.size shouldBeUGroup `shouldBe` (length lst)
      it "should return an empty set from an empty file" $ do
        shouldBeNoUsers <- readUserCache userCacheEmptyPath
        let shouldBeUGroup = (createUserGroup $ head $ rights [shouldBeNoUsers])
        Set.null shouldBeUGroup `shouldBe` True
      it "should return a singleton set from a singleton usercache.json" $ do
        shouldBeOneUser <- readUserCache userCacheSingletonPath
        let shouldBeUGroup = (createUserGroup.head $ rights [shouldBeOneUser])
        Set.size shouldBeUGroup `shouldBe` (1 :: Int)

    describe "OpenSandbox.Minecraft.User.writeUserCache" $ do
      it "should write out that which was just read" $ do
        input1 <- readUserCache userCachePath
        writeUserCache (testSrvDir ++ "/" ++ "newusercache.json") (head $ rights [input1])
        input2 <- readUserCache (testSrvDir ++ "/" ++ "newusercache.json")
        (input1 == input2) `shouldBe` True
        removeFile (testSrvDir ++ "/" ++ "newusercache.json")

    describe "OpenSandbox.Tmux.parseTmuxID" $ do
      it "should build a TmuxID from a String ++ ':' ++ String" $ do
        isJust (parseTmuxID "opensandbox:25565") `shouldBe` True
      it "should return Nothing if it is given a string that has more or less than 1 ':'" $ do
        parseTmuxID "opensandbox25565" `shouldBe` Nothing
        parseTmuxID "opensandbox::25565" `shouldBe` Nothing
      it "should return Nothing if either the sessionID or the windowID are an empty string" $ do
        parseTmuxID ":25565" `shouldBe` Nothing
        parseTmuxID "opensandbox:" `shouldBe` Nothing
