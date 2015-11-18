import Data.Either
import OpenSandbox
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

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
