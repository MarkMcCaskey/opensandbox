{-# LANGUAGE OverloadedStrings #-}
import            Control.Monad
import qualified  Data.ByteString as B
import            Data.Either
import            Data.Int
import            Data.Serialize
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID
import qualified  Data.Vector as V
import            Data.Word
import            OpenSandbox
import            Test.Hspec
import            Test.QuickCheck

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList $ listOf arbitrary


instance Arbitrary UUID where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ fromWords a b c d


instance Arbitrary Difficulty where
  arbitrary = elements [Peaceful,Easy,Normal,Hard]


instance Arbitrary GameMode where
  arbitrary = elements [Survival,Creative,Adventure,Spectator]


instance Arbitrary Dimension where
  arbitrary = elements [Overworld,Nether,End]


instance Arbitrary WorldType where
  arbitrary = elements [Default,Flat,LargeBiomes,Amplified]


instance Arbitrary Statistic where
  arbitrary = do
    a <- arbitrary :: Gen T.Text
    b <- arbitrary :: Gen Int
    return $ Statistic a b


instance Arbitrary Player where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Player a b


instance Arbitrary NextState where
  arbitrary = do
    switch <- choose (1,2) :: Gen Int
    case switch of
      1 -> return ProtocolStatus
      2 -> return ProtocolLogin


instance Arbitrary PlayerListAction where
  arbitrary = do
    switch <- choose (0,4) :: Gen Int
    case switch of
      0 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ PlayerListAdd a b c d e
      1 -> do
        a <- arbitrary
        return $ PlayerListUpdateGameMode a
      2 -> do
        a <- arbitrary
        return $ PlayerListUpdateLatency a
      3 -> do
        a <- arbitrary
        b <- arbitrary
        return $ PlayerListUpdateDisplayName a b
      4 -> do
        return $ PlayerListRemovePlayer


instance Arbitrary PlayerProperty where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ PlayerProperty a b c d


instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary


instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary


instance Arbitrary ServerBoundHandshaking where
  arbitrary = do
    v <- arbitrary
    a <- arbitrary
    p <- arbitrary
    s <- arbitrary
    return $ SBHandshake v a p s


instance Arbitrary ServerBoundStatus where
  arbitrary = do
    packetID <- elements [0..1] :: Gen Int
    case packetID of

      0x00 -> do
        return ServerBoundRequest

      0x01 -> do
        a <- arbitrary
        return $ ServerBoundPing a


instance Arbitrary ClientBoundStatus where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Word8
    case packetID of

      0x00  -> do
        a <- arbitrary
        return $ ClientBoundResponse a

      0x01  -> do
        a <- arbitrary
        return $ ClientBoundPong a


instance Arbitrary ClientBoundLogin where
  arbitrary = do
    packetID <- elements [0..3] :: Gen Int
    case packetID of

      0 -> do
        a <- arbitrary
        return $ ClientBoundLoginDisconnect a

      1 -> do
        a <- arbitrary
        b <- fmap B.pack arbitrary
        c <- fmap B.pack arbitrary
        return $ ClientBoundEncryptionRequest a b c

      2 -> do
        a <- arbitrary
        b <- arbitrary
        return $ ClientBoundLoginSuccess a b

      3 -> do
        a <- arbitrary
        return $ ClientBoundSetCompression a


instance Arbitrary ServerBoundLogin where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Int
    case packetID of

      0 -> do
        a <- arbitrary
        return $ ServerBoundLoginStart a

      1 -> do
        a <- fmap B.pack arbitrary
        b <- fmap B.pack arbitrary
        return $ ServerBoundEncryptionResponse a b


prop_ServerBoundHandshakingEq :: [ServerBoundHandshaking] -> Bool
prop_ServerBoundHandshakingEq lst =
  lst == (rights (map decode $ map encode lst :: [Either String ServerBoundHandshaking]))


prop_ClientBoundStatusEq :: [ClientBoundStatus] -> Bool
prop_ClientBoundStatusEq lst =
  lst == (rights (map decode $ map encode lst :: [Either String ClientBoundStatus]))


prop_ServerBoundStatusEq :: [ServerBoundStatus] -> Bool
prop_ServerBoundStatusEq lst =
  lst == (rights (map decode $ map encode lst :: [Either String ServerBoundStatus]))


prop_ClientBoundLoginEq :: [ClientBoundLogin] -> Bool
prop_ClientBoundLoginEq lst =
  lst == (rights (map decode $ map encode lst :: [Either String ClientBoundLogin]))


prop_ServerBoundLoginEq :: [ServerBoundLogin] -> Bool
prop_ServerBoundLoginEq lst =
  lst == (rights (map decode $ map encode lst :: [Either String ServerBoundLogin]))


main :: IO ()
main = hspec $ do
  describe "Minecraft Protocol" $ do
    context "Server bound handshaking packets:" $ do
      it "Identity" $ property prop_ServerBoundHandshakingEq
{-
    context "Client bound status packets:" $ do
      it "Identity" $ property prop_ClientBoundStatusEq
    context "Server bound status packets:" $ do
      it "Identity" $ property prop_ServerBoundStatusEq
    context "Client bound login packets:" $ do
      it "Identity" $ property prop_ClientBoundLoginEq
    context "Server bound login packets:" $ do
      it "Identity" $ property prop_ServerBoundLoginEq
      -}
