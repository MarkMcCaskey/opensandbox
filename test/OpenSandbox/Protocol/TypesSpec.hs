{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module OpenSandbox.Protocol.TypesSpec (main,spec) where

import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import qualified Data.Text as T
import Data.UUID
import qualified Data.Vector as V
import Data.Word
import OpenSandbox
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Test
import OpenSandbox.WorldSpec()
import Common
import Data.NBTSpec()

instance Arbitrary UpdatedColumns where
  arbitrary = do
    columns <- (arbitrary :: Gen Int8)
    if columns > 0
      then UpdatedColumns <$> return columns <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      else return NoUpdatedColumns

instance Arbitrary UpdateScoreAction where
  arbitrary = do
    action <- arbitrary :: Gen Bool
    if action
      then CreateOrUpdateScoreItem <$> arbitrary <*> arbitrary <*> arbitrary
      else RemoveScoreItem <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList $ listOf arbitrary

instance Arbitrary UUID where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ fromWords a b c d

instance Arbitrary TitleAction where
  arbitrary = do
    k <- choose (1,5) :: Gen Int
    case k of
      1 -> SetTitle <$> arbitrary
      2 -> SetSubtitle <$> arbitrary
      3 -> SetTimesAndDisplay <$> arbitrary <*> arbitrary <*> arbitrary
      4 -> return Hide
      5 -> return Reset
      _ -> fail "Error: This should not be possible!"

instance Arbitrary TeamMode where
  arbitrary = do
    k <- choose (1,5) :: Gen Int
    case k of
      1 -> CreateTeam
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
      2 -> return $ RemoveTeam
      3 -> UpdateTeamInfo
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
      4 -> AddPlayers <$> arbitrary
      5 -> RemovePlayers <$> arbitrary
      _ -> fail "Error: This should not be possible!"

instance Arbitrary EntityProperty where
  arbitrary = EntityProperty <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary WorldBorderAction where
  arbitrary = do
    k <- choose (1,6) :: Gen Int
    case k of
      1 -> SetSize <$> arbitrary
      2 -> LerpSize <$> arbitrary <*> arbitrary <*> arbitrary
      3 -> SetCenter <$> arbitrary <*> arbitrary
      4 -> Initialize
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
      5 -> SetWarningTime <$> arbitrary
      6 -> SetWarningBlocks <$> arbitrary
      _ -> fail "Error: This should not be possible!"

instance Arbitrary Icon where
  arbitrary = Icon <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CombatEvent where
  arbitrary = do
    k <- choose (1,3) :: Gen Int
    case k of
      1 -> return $ EnterCombat
      2 -> EndCombat <$> arbitrary <*> arbitrary
      3 -> EntityDead <$> arbitrary <*> arbitrary <*> arbitrary
      _ -> fail "Error: This should not be possible!"

instance Arbitrary EntityStatus where
  arbitrary = fmap toEnum (choose (0,34) :: Gen Int)

instance Arbitrary BlockChange where
  arbitrary = BlockChange <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary GameChangeReason where
  arbitrary = do
    i <- (choose (0,10) :: Gen Int)
    if i /= 9
      then return (toEnum i)
      else return (toEnum (i - 1))

instance Arbitrary Difficulty where
  arbitrary = fmap toEnum (choose (0,3) :: Gen Int)

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

instance Arbitrary ProtocolState where
  arbitrary = do
    switch <- choose (1,2) :: Gen Int
    case switch of
      1 -> return ProtocolStatus
      2 -> return ProtocolLogin
      _ -> fail "Error: This should not be possible!"

instance Arbitrary PlayerListEntries where
  arbitrary = do
    i <- choose (0,4) :: Gen Int
    case i of
      0 -> PlayerListAdds <$> arbitrary
      1 -> PlayerListUpdateGameModes <$> arbitrary
      2 -> PlayerListUpdateLatencies <$> arbitrary
      3 -> PlayerListUpdateDisplayNames <$> arbitrary
      4 -> PlayerListRemovePlayers <$> arbitrary
      _ -> fail "Error: This should not be possible!"

instance Arbitrary PlayerListAdd where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    return $ PlayerListAdd a b c d e f

instance Arbitrary PlayerListUpdateGameMode where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ PlayerListUpdateGameMode a b

instance Arbitrary PlayerListUpdateLatency where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ PlayerListUpdateLatency a b

instance Arbitrary PlayerListUpdateDisplayName where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ PlayerListUpdateDisplayName a b

instance Arbitrary PlayerListRemovePlayer where
  arbitrary = do
    a <- arbitrary
    return $ PlayerListRemovePlayer a

instance Arbitrary PlayerProperty where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ PlayerProperty a b c

instance Arbitrary EntityMetadataEntry where
  arbitrary = do
    k <- arbitrary :: Gen Bool
    w <- choose (0,254) :: Gen Word8
    if k
      then return MetadataEnd
      else Entry <$> return w <*> arbitrary <*> arbitrary

instance Arbitrary MetadataType where
  arbitrary = fmap toEnum (choose (0,12) :: Gen Int)

instance Arbitrary Animation where
  arbitrary = fmap toEnum (choose (0,5) :: Gen Int)

instance Arbitrary UpdateBlockEntityAction where
  arbitrary = fmap toEnum (choose (1,9) :: Gen Int)

instance Arbitrary BlockAction where
  arbitrary = do
    k <- choose (0,2) :: Gen Int
    case k of
      0 -> NoteBlockAction <$> arbitrary <*> arbitrary
      1 -> PistonBlockAction <$> arbitrary <*> arbitrary
      2 -> ChestBlockAction <$> arbitrary
      _ -> fail "Error: This should not be possible!"

instance Arbitrary InstrumentType where
  arbitrary = fmap toEnum (choose (0,4) :: Gen Int)

instance Arbitrary NotePitch where
  arbitrary = fmap toEnum (choose (0,24) :: Gen Int)

instance Arbitrary PistonState where
  arbitrary = fmap toEnum (choose (0,1) :: Gen Int)

instance Arbitrary PistonDirection where
  arbitrary = fmap toEnum (choose (0,5) :: Gen Int)

instance Arbitrary BossBarAction where
  arbitrary = do
    k <- choose (0,5) :: Gen Int
    case k of
      0 -> BossBarAdd <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      1 -> return BossBarRemove
      2 -> BossBarUpdateHealth <$> arbitrary
      3 -> BossBarUpdateTitle <$> arbitrary
      4 -> BossBarUpdateStyle <$> arbitrary <*> arbitrary
      5 -> BossBarUpdateFlags <$> arbitrary
      _ -> fail "Error: This should not be possible!"

instance Arbitrary Slot where
  arbitrary = mkSlot <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UseEntityType where
  arbitrary = do
    t <- (choose (0,2) :: Gen Int)
    case t of
      0 -> InteractWithEntity <$> arbitrary
      1 -> return AttackEntity
      2 -> InteractAtEntity <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      _ -> fail "Error: This should not be possible!"

instance Arbitrary EntityHand where
  arbitrary = fmap toEnum (choose (0,1) :: Gen Int)

instance Arbitrary ScoreboardMode where
  arbitrary = do
    k <- choose (0,2) :: Gen Int
    case k of
      0 -> CreateScoreboard <$> arbitrary <*> arbitrary
      1 -> return RemoveScoreboard
      2 -> UpdateDisplayText <$> arbitrary <*> arbitrary
      _ -> fail "Error: This should not be possible!"

spec :: Spec
spec = do
  describe "Minecraft Protocol Core Types (Serialize)" $ do
    context "VarInt:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_CustomSerializeIdentity putVarInt getVarInt :: Int -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "VarLong:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_CustomSerializeIdentity putVarLong getVarLong :: Int64 -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "String:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_CustomSerializeIdentity putText getText :: T.Text -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "EntityMetadata:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_CustomSerializeIdentity putEntityMetadata getEntityMetadata :: EntityMetadata -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Slot:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: Slot -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Position:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_CustomSerializeIdentity putPosition getPosition :: Position -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Angle:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_CustomSerializeIdentity putAngle getAngle :: Angle -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "UUID:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_CustomSerializeIdentity putUUID getUUID :: UUID -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "UUID':" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_CustomSerializeIdentity putUUID' getUUID' :: UUID -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "ByteArray:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_CustomSerializeIdentity putNetcodeByteString getNetcodeByteString :: B.ByteString -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
  describe "Minecraft Protocol Custom Records" $ do
    context "BlockChange:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: BlockChange -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Statistic:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: Statistic -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "PlayerProperty:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: PlayerProperty -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "PlayerListEntries:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: PlayerListEntries -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Icon:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: Icon -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "EntityProperty:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: EntityProperty -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "BlockAction:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: BlockAction -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "BossBar:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: BossBarAction -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "WorldBorderAction:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: WorldBorderAction -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "UpdatedColumns:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: UpdatedColumns -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "CombatEvent:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: CombatEvent -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "ScoreboardMode:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: ScoreboardMode -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "TeamMode:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: TeamMode -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "TitleAction:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: TitleAction -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True

main :: IO ()
main = hspec spec
