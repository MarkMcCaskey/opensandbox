module OpenSandbox.Protocol.PacketSpec (main,spec) where
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
import            Control.Monad
import qualified  Data.Array.IArray as IA
import            Data.Array.Unboxed (listArray)
import qualified  Data.Attoparsec.ByteString as Decode
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as Encode
import            Data.Either
import            Data.Int
import            Data.Monoid
import            Data.NBT
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID
import qualified  Data.Vector as V
import qualified  Data.Vector.Unboxed as U
import            Data.Word
import            OpenSandbox
import            Test.Hspec
import            Test.QuickCheck
import            GHC.Generics
import            Debug.Trace

import OpenSandbox.WorldSpec()
import OpenSandbox.Protocol.TypesSpec()

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
    packetID <- choose (0x00,0x03) :: Gen Word8
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
    packetID <- choose (0x00,0x01) :: Gen Word8
    case packetID of

      0 -> do
        a <- arbitrary
        return $ SBLoginStart a

      1 -> do
        a <- fmap B.pack arbitrary
        b <- fmap B.pack arbitrary
        return $ SBEncryptionResponse a b

instance Arbitrary CBPlay where
  arbitrary = do
    packetID <- choose (0x00,0x4B) :: Gen Word8
    case packetID of
      0x00 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        i <- arbitrary
        j <- arbitrary
        k <- arbitrary
        l <- arbitrary
        return $ CBSpawnObject a b c d e f g h i j k l

      0x01 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBSpawnExperienceOrb a b c d e

      0x02 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBSpawnGlobalEntity a b c d e

      0x03 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        i <- arbitrary
        j <- arbitrary
        k <- arbitrary
        l <- arbitrary
        m <- arbitrary
        return $ CBSpawnMob a b c d e f g h i j k l m

      0x04 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBSpawnPainting a b c d e

      0x05 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        return $ CBSpawnPlayer a b c d e f g h

      0x06 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBAnimation a b

      0x07 -> do
        a <- arbitrary
        return $ CBStatistics a

      0x08 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBBlockBreakAnimation a b c

      0x09 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBUpdateBlockEntity a b c

      0x0A -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBBlockAction a b

      0x0B -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBBlockChange a b

      0x0C -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBBossBar a b

      0x0D -> do
        a <- arbitrary
        return $ CBServerDifficulty a

      0x0E -> do
        a <- arbitrary
        return $ CBTabComplete a

      0x0F -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBChatMessage a b

      0x10 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBMultiBlockChange a b c

      0x11 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBConfirmTransaction a b c

      0x12 -> do
        a <- arbitrary
        return $ CBCloseWindow a

      0x13 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBOpenWindow a b c d

      0x14 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBWindowItems a b

      0x15 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBWindowProperty a b c

      0x16 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBSetSlot a b c

      0x17 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBSetCooldown a b

      0x18 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBPluginMessage a b

      0x19 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBNamedSoundEffect a b c d e f g

      0x1A -> do
        a <- arbitrary
        return $ CBPlayDisconnect a

      0x1B -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBEntityStatus a b

      0x1C -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        return $ CBExplosion a b c d e f g h

      0x1D -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBUnloadChunk a b

      0x1E -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBChangeGameState a b

      0x1F -> do
        a <- arbitrary
        return $ CBKeepAlive a

      0x20 -> do
        --a <- arbitrary
        --b <- arbitrary
        --c <- arbitrary
        d <- arbitrary
        {-
        rand <- arbitrary :: Gen Bool
        e <- case rand of
              True -> fmap (Just . B.pack) $ vectorOf 256 (arbitrary :: Gen Word8)
              False -> return Nothing
        -}
        --f <- arbitrary
        return $ CBChunkData {-a b c-} d {-e-} {-f-}

      0x21 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBEffect a b c d

      0x22 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        h <- arbitrary
        i <- arbitrary
        j <- arbitrary
        k <- arbitrary
        return $ CBParticle a b c d e f g h i j k

      0x23 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBJoinGame a b c d e f g

      0x24 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBMap a b c d e

      0x25 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBEntityRelativeMove a b c d e

      0x26 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBEntityLookAndRelativeMove a b c d e f g

      0x27 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBEntityLook a b c d

      0x28 -> do
        a <- arbitrary
        return $ CBEntity a

      0x29 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBVehicleMove a b c d e

      0x2A -> do
        a <- arbitrary
        return $ CBOpenSignEditor a

      0x2B -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBPlayerAbilities a b c

      0x2C -> do
        a <- arbitrary
        return $ CBCombatEvent a

      0x2D -> do
        a <- arbitrary
        return $ CBPlayerListItem a

      0x2E -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBPlayerPositionAndLook a b c d e f g

      0x2F -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBUseBed a b

      0x30 -> do
        a <- arbitrary
        return $ CBDestroyEntities a

      0x31 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBRemoveEntityEffect a b

      0x32 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBResourcePackSend a b

      0x33 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBRespawn a b c d

      0x34 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBEntityHeadLook a b

      0x35 -> do
        a <- arbitrary
        return $ CBWorldBorder a

      0x36 -> do
        a <- arbitrary
        return $ CBCamera a

      0x37 -> do
        a <- arbitrary
        return $ CBHeldItemChange a

      0x38 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBDisplayScoreboard a b

      0x39 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBEntityMetadata a b

      0x3A -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBAttachEntity a b

      0x3B -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ CBEntityVelocity a b c d

      0x3C -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBEntityEquipment a b c

      0x3D -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBSetExperience a b c

      0x3E -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ CBUpdateHealth a b c

      0x3F -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBScoreboardObjective a b

      0x40 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBSetPassengers a b

      0x41 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBTeams a b

      0x42 -> do
        a <- arbitrary
        return $ CBUpdateScore a

      0x43 -> do
        a <- arbitrary
        return $ CBSpawnPosition a

      0x44 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBTimeUpdate a b

      0x45 -> do
        a <- arbitrary
        return $ CBTitle a

      0x46 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBSoundEffect a b c d e f g

      0x47 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBPlayerListHeaderAndFooter a b

      0x48 -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBCollectItem a b

      0x49 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        g <- arbitrary
        return $ CBEntityTeleport a b c d e f g

      0x4A -> do
        a <- arbitrary
        b <- arbitrary
        return $ CBEntityProperties a b

      0x4B -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ CBEntityEffect a b c d e

instance Arbitrary SBPlay where
  arbitrary = do
    packetID <- choose (0x00,0x1D) :: Gen Word8
    case packetID of
      0x00 -> do
        a <- arbitrary
        return $ SBTeleportConfirm a

      0x01 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBTabComplete a b c

      0x02 -> do
        a <- arbitrary
        return $ SBChatMessage a

      0x03 -> do
        a <- arbitrary
        return $ SBClientStatus a

      0x04 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        return $ SBClientSettings a b c d e f

      0x05 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBConfirmTransaction a b c

      0x06 -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBEnchantItem a b

      0x07 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        return $ SBClickWindow a b c d e f

      0x08 -> do
        a <- arbitrary
        return $ SBCloseWindow a

      0x09 -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBPluginMessage a b

      0x0A -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBUseEntity a b

      0x0B -> do
        a <- arbitrary
        return $ SBKeepAlive a

      0x0C -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ SBPlayerPosition a b c d

      0x0D -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        return $ SBPlayerPositionAndLook a b c d e f

      0x0E -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBPlayerLook a b c

      0x0F -> do
        a <- arbitrary
        return $ SBPlayer a

      0x10 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ SBVehicleMove a b c d e

      0x11 -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBSteerBoat a b

      0x12 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBPlayerAbilities a b c

      0x13 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBPlayerDigging a b c

      0x14 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBEntityAction a b c

      0x15 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ SBSteerVehicle a b c

      0x16 -> do
        a <- arbitrary
        return $ SBResourcePackStatus a

      0x17 -> do
        a <- arbitrary
        return $ SBHeldItemChange a

      0x18 -> do
        a <- arbitrary
        b <- arbitrary
        return $ SBCreativeInventoryAction a b

      0x19 -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ SBUpdateSign a b c d e

      0x1A -> do
        a <- arbitrary
        return $ SBAnimation a

      0x1B -> do
        a <- arbitrary
        return $ SBSpectate a

      0x1C -> do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        f <- arbitrary
        return $ SBPlayerBlockPlacement a b c d e f

      0x1D -> do
        a <- arbitrary
        return $ SBUseItem a

prop_SBHandshakingEq :: [SBHandshaking] -> Bool
prop_SBHandshakingEq [] = True
prop_SBHandshakingEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeSBHandshaking x)) lst
  let decoded = fmap (Decode.parseOnly decodeSBHandshaking) encoded :: [Either String SBHandshaking]
  lst == (rights decoded)

prop_CBStatusEq :: [CBStatus] -> Bool
prop_CBStatusEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeCBStatus x)) lst
  let decoded = fmap (Decode.parseOnly decodeCBStatus) encoded :: [Either String CBStatus]
  lst == (rights decoded)

prop_SBStatusEq :: [SBStatus] -> Bool
prop_SBStatusEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeSBStatus x)) lst
  let decoded = fmap (Decode.parseOnly decodeSBStatus) encoded :: [Either String SBStatus]
  lst == (rights decoded)

prop_CBLoginEq :: [CBLogin] -> Bool
prop_CBLoginEq [] = True
prop_CBLoginEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeCBLogin x)) lst
  let decoded = fmap (Decode.parseOnly decodeCBLogin) encoded :: [Either String CBLogin]
  lst == (rights decoded)

prop_SBLoginEq :: [SBLogin] -> Bool
prop_SBLoginEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeSBLogin x)) lst
  let decoded = fmap (Decode.parseOnly decodeSBLogin) encoded :: [Either String SBLogin]
  lst == (rights decoded)

prop_CBPlayEq :: [CBPlay] -> Bool
prop_CBPlayEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeCBPlay x)) lst
  let decoded = fmap (Decode.parseOnly decodeCBPlay) encoded :: [Either String CBPlay]
  lst == (rights decoded)

prop_SBPlayEq :: [SBPlay] -> Bool
prop_SBPlayEq [] = True
prop_SBPlayEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeSBPlay x)) lst
  let decoded = fmap (Decode.parseOnly decodeSBPlay) encoded :: [Either String SBPlay]
  lst == (rights decoded)

spec :: Spec
spec = do
  describe "Minecraft Protocol Packets" $ do
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
    context "Client bound play packets:" $ do
      it "Identity" $ property prop_CBPlayEq
    context "Server bound play packets:" $ do
      it "Identity" $ property prop_SBPlayEq

main :: IO ()
main = hspec spec
