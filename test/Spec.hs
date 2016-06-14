{-# LANGUAGE OverloadedStrings #-}
import            Control.Monad
import            Data.Attoparsec.ByteString
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as BB
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


instance Arbitrary SBHandshaking where
  arbitrary = do
    v <- arbitrary
    a <- arbitrary
    p <- arbitrary
    s <- arbitrary
    return $ SBHandshake v a p s


instance Arbitrary SBStatus where
  arbitrary = do
    packetID <- elements [0..1] :: Gen Int
    case packetID of

      0x00 -> do
        return SBRequest

      0x01 -> do
        a <- arbitrary
        return $ SBPing a


instance Arbitrary CBStatus where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Word8
    case packetID of

      0x00  -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBResponse a b c d e

      0x01  -> do
        a <- arbitrary
        return $ CBPong a


instance Arbitrary CBLogin where
  arbitrary = do
    packetID <- elements [0..3] :: Gen Int
    case packetID of

      0 -> do
        a <- arbitrary
        return $ CBLoginDisconnect a

      1 -> do
        a <- arbitrary
        b <- fmap B.pack arbitrary
        c <- fmap B.pack arbitrary
        return $ CBEncryptionRequest a b c

      2 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBLoginSuccess a b

      3 -> do
        a <- arbitrary
        return $ CBSetCompression a


instance Arbitrary SBLogin where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Int
    case packetID of

      0 -> do
        a <- arbitrary
        return $ SBLoginStart a

      1 -> do
        a <- fmap B.pack arbitrary
        b <- fmap B.pack arbitrary
        return $ SBEncryptionResponse a b


prop_SBHandshakingEq :: [SBHandshaking] -> Bool
prop_SBHandshakingEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeSBHandshaking x)) lst
  let decoded = fmap (parseOnly decodeSBHandshaking) encoded :: [Either String SBHandshaking]
  lst == (rights decoded)


prop_CBStatusEq :: [CBStatus] -> Bool
prop_CBStatusEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeCBStatus x)) lst
  let decoded = fmap (parseOnly decodeCBStatus) encoded :: [Either String CBStatus]
  lst == (rights decoded)


prop_SBStatusEq :: [SBStatus] -> Bool
prop_SBStatusEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeSBStatus x)) lst
  let decoded = fmap (parseOnly decodeSBStatus) encoded :: [Either String SBStatus]
  lst == (rights decoded)


prop_CBLoginEq :: [CBLogin] -> Bool
prop_CBLoginEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeCBLogin x)) lst
  let decoded = fmap (parseOnly decodeCBLogin) encoded :: [Either String CBLogin]
  lst == (rights decoded)


prop_SBLoginEq :: [SBLogin] -> Bool
prop_SBLoginEq lst = do
  let encoded = fmap (\x -> BL.toStrict . BB.toLazyByteString $ (encodeSBLogin x)) lst
  let decoded = fmap (parseOnly decodeSBLogin) encoded :: [Either String SBLogin]
  lst == (rights decoded)


main :: IO ()
main = hspec $ do
  describe "Minecraft Protocol" $ do
    context "Server bound handshaking packets:" $ do
      it "Identity" $ property prop_SBHandshakingEq
    context "Client bound status packets:" $ do
      it "Identity" $ property prop_CBStatusEq
    context "Server bound status packets:" $ do
      it "Identity" $ property prop_SBStatusEq
    context "Client bound login packets:" $ do
      it "Identity" $ property prop_CBLoginEq
    context "Server bound login packets:" $ do
      it "Identity" $ property prop_SBLoginEq
