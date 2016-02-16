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


instance Arbitrary Stat where
  arbitrary = do
    a <- arbitrary :: Gen T.Text
    b <- arbitrary :: Gen Int
    return $ Stat a b


instance Arbitrary Player where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Player a b


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


instance Arbitrary ServerBoundStatus where
  arbitrary = do
    packetID <- elements [0..2] :: Gen Int
    case packetID of

      0x00 -> do
        v <- arbitrary
        a <- arbitrary
        p <- arbitrary
        s <- arbitrary
        return $ ServerBoundHandshake v a p s

      0x01 -> do
        return ServerBoundPingStart

      0x02 -> do
        payload <- arbitrary
        return $ ServerBoundPing payload


instance Arbitrary ClientBoundStatus where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Word8
    case packetID of

      0x00  -> do
        a <- fmap T.pack arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- fmap T.pack arbitrary
        return $ ClientBoundResponse a b c d e

      0x01  -> do
        pong <- arbitrary
        return $ ClientBoundPong pong


instance Arbitrary ClientBoundLogin where
  arbitrary = do
    packetID <- elements [0..3] :: Gen Int
    case packetID of

      0 -> do
        payload <- fmap B.pack arbitrary
        return $ ClientBoundDisconnect payload

      1 -> do
        a <- fmap B.pack arbitrary
        b <- fmap B.pack arbitrary
        c <- fmap B.pack arbitrary
        return $ ClientBoundEncryptionRequest a b c

      2 -> do
        a <- fmap B.pack arbitrary
        b <- fmap B.pack arbitrary
        return $ ClientBoundLoginSuccess a b

      3 -> do
        a <- arbitrary
        return $ ClientBoundSetCompression a


instance Arbitrary ServerBoundLogin where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Int
    case packetID of

      0 -> do
        a <- fmap B.pack arbitrary
        return $ ServerBoundLoginStart a

      1 -> do
        a <- fmap B.pack arbitrary
        b <- fmap B.pack arbitrary
        return $ ServerBoundEncryptionResponse a b


instance Arbitrary ClientBoundPlay where
  arbitrary = do
    packetID <- elements [0x07,0x0d,0x18,0x1b,0x1f,0x2b,0x2d,0x37,0x43] :: Gen Word8
    case packetID of

      0x07 -> do
        a <- fmap V.fromList (arbitrary :: Gen [Stat])
        return $ ClientBoundStatistics a

      0x0d -> do
        a <- arbitrary
        return $ ClientBoundDifficulty a

      0x18 -> do
        a <- arbitrary
        b <- arbitrary
        return $ ClientBoundCustomPayload a b

      0x1b -> do
        a <- arbitrary
        b <- arbitrary
        return $ ClientBoundEntityStatus a b

      0x1f -> do
        a <- arbitrary
        return $ ClientBoundKeepAlive a

      0x23 -> do
        a <- (fmap toEnum arbitrary) :: Gen Int32
        b <- arbitrary :: Gen GameMode
        c <- arbitrary :: Gen Dimension
        d <- arbitrary :: Gen Difficulty
        e <- arbitrary :: Gen Word8
        f <- arbitrary :: Gen WorldType
        g <- arbitrary :: Gen Bool
        return $ ClientBoundLogin a b c d e f g

      0x2b -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ ClientBoundPlayerAbilities a b c

      0x2d -> do
        a <- arbitrary
        b <- arbitrary
        return $ ClientBoundPlayerListItem a b

      0x37 -> do
        a <- arbitrary
        return $ ClientBoundHeldItemSlot a

      0x43 -> do
        a <- arbitrary
        b <- arbitrary
        return $ ClientBoundUpdateTime a b


instance Arbitrary ServerBoundPlay where
  arbitrary = do
    packetID <- elements [0x0b]
    case packetID of
      0x0b -> do
        a <- arbitrary
        return $ ServerBoundKeepAlive a


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


prop_ClientBoundPlayEq :: [ClientBoundPlay] -> Bool
prop_ClientBoundPlayEq lst =
  lst == (rights (map decode $ map encode lst :: [Either String ClientBoundPlay]))


prop_ServerBoundPlayEq :: [ServerBoundPlay] -> Bool
prop_ServerBoundPlayEq lst =
  lst == (rights (map decode $ map encode lst :: [Either String ServerBoundPlay]))


main :: IO ()
main = do
  quickCheck prop_ClientBoundStatusEq
  quickCheck prop_ServerBoundStatusEq
  quickCheck prop_ClientBoundLoginEq
  quickCheck prop_ServerBoundLoginEq
  quickCheck prop_ClientBoundPlayEq
  quickCheck prop_ServerBoundPlayEq
