{-# LANGUAGE OverloadedStrings #-}
import            Control.Monad
import qualified  Data.ByteString as B
import            Data.Either
import            Data.Serialize
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.Word
import            OpenSandbox
import            Test.QuickCheck


instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

instance Arbitrary ServerBoundStatus where
  arbitrary = do
    packetID <- elements [0..2] :: Gen Int
    case packetID of
      0 -> do
        v <- arbitrary
        a <- arbitrary
        p <- arbitrary
        s <- arbitrary
        return $ ServerBoundHandshake v a p s
      1 -> return ServerBoundPingStart
      2 -> do payload <- arbitrary
              return $ ServerBoundPing payload


instance Arbitrary ClientBoundStatus where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Word8
    case packetID of
      0x00  -> do mcversion <- fmap T.pack arbitrary
                  versionID <- arbitrary
                  currentPlayers <- arbitrary
                  maxPlayers <- arbitrary
                  motd <- fmap T.pack arbitrary
                  return $ ClientBoundResponse
                        mcversion
                        versionID
                        currentPlayers
                        maxPlayers
                        motd
      0x01  -> do pong <- arbitrary
                  return $ ClientBoundPong pong


instance Arbitrary ClientBoundLogin where
  arbitrary = do
    packetID <- elements [0..3] :: Gen Int
    case packetID of
      0 -> do payload <- fmap B.pack arbitrary
              return $ ClientBoundDisconnect payload
      1 -> do a <- fmap B.pack arbitrary
              b <- fmap B.pack arbitrary
              c <- fmap B.pack arbitrary
              return $ ClientBoundEncryptionRequest a b c
      2 -> do a <- fmap B.pack arbitrary
              b <- fmap B.pack arbitrary
              return $ ClientBoundLoginSuccess a b
      3 -> do a <- arbitrary
              return $ ClientBoundSetCompression a


instance Arbitrary ServerBoundLogin where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Int
    case packetID of
      0 -> do a <- fmap B.pack arbitrary
              return $ ServerBoundLoginStart a
      1 -> do a <- fmap B.pack arbitrary
              b <- fmap B.pack arbitrary
              return $ ServerBoundEncryptionResponse a b


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
main = do
  quickCheck prop_ClientBoundStatusEq
  quickCheck prop_ServerBoundStatusEq
  quickCheck prop_ClientBoundLoginEq
  quickCheck prop_ServerBoundLoginEq
