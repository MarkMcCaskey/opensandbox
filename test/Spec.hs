import            Control.Monad
import qualified  Data.ByteString as B
import            Data.Serialize
import            Data.Word
import            OpenSandbox
import            Test.QuickCheck
import            Test.Hspec

instance Arbitrary ServerBoundStatus where
  arbitrary = do
    packetID <- elements [0..3] :: Gen Int
    case packetID of
      0 -> do v <- arbitrary :: Gen Word8
              a <- fmap B.pack arbitrary :: Gen B.ByteString
              p <- arbitrary
              s <- arbitrary
              return $ ServerBoundHandshake v a p s
      1 -> return ServerBoundPingStart
      2 -> do payload <- arbitrary
              return $ ServerBoundPing payload
      3 -> return ServerBoundRequest


instance Arbitrary ClientBoundStatus where
  arbitrary = do
    packetID <- choose (0,1) :: Gen Int
    case packetID of
      0 -> do response <- fmap B.pack arbitrary
              return $ ClientBoundResponse response
      1 -> do pong <- arbitrary
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

main :: IO ()
main = do
  print "----------------------------------------------------------------------"
  sample (arbitrary :: Gen ClientBoundStatus)
  print "----------------------------------------------------------------------"
  sample (arbitrary :: Gen ServerBoundStatus)
  print "----------------------------------------------------------------------"
  sample (arbitrary :: Gen ClientBoundLogin)
  print "----------------------------------------------------------------------"
  sample (arbitrary :: Gen ServerBoundLogin)
  print "----------------------------------------------------------------------"
