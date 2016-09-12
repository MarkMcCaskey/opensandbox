{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module OpenSandbox.Protocol.TypesSpec (main,spec) where

import qualified  Data.ByteString as B
import            Data.Int
import qualified  Data.Text as T
import            Data.UUID
import qualified  Data.Vector as V
import            Data.Word
import            OpenSandbox
import            Test.Hspec
import            Test.QuickCheck
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

instance Arbitrary Icon where
  arbitrary = Icon <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CombatEvent where
  arbitrary = do
    k <- choose (1,3) :: Gen Int
    case k of
      1 -> return $ EnterCombat
      2 -> EndCombat <$> arbitrary <*> arbitrary
      3 -> EntityDead <$> arbitrary <*> arbitrary <*> arbitrary

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

instance Arbitrary PlayerListEntries where
  arbitrary = do
    i <- choose (0,4) :: Gen Int
    case i of
      0 -> PlayerListAdds <$> arbitrary
      1 -> PlayerListUpdateGameModes <$> arbitrary
      2 -> PlayerListUpdateLatencies <$> arbitrary
      3 -> PlayerListUpdateDisplayNames <$> arbitrary
      4 -> PlayerListRemovePlayers <$> arbitrary

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

instance Arbitrary Slot where
  arbitrary = mkSlot <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UseEntityType where
  arbitrary = do
    t <- (choose (0,2) :: Gen Int)
    case t of
      0 -> InteractWithEntity <$> arbitrary
      1 -> return AttackEntity
      2 -> InteractAtEntity <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary EntityHand where
  arbitrary = fmap toEnum (choose (0,1) :: Gen Int)

instance Arbitrary ScoreboardMode where
  arbitrary = do
    k <- choose (0,2) :: Gen Int
    case k of
      0 -> CreateScoreboard <$> arbitrary <*> arbitrary
      1 -> return RemoveScoreboard
      2 -> UpdateDisplayText <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "Minecraft Protocol Core Types (Serialize)" $ do
    context "VarInt:" $ do
      it "Identity" $ property (prop_CustomSerializeIdentity putVarInt getVarInt :: Int -> Bool)
    context "VarLong:" $ do
      it "Identity" $ property (prop_CustomSerializeIdentity putVarLong getVarLong :: Int64 -> Bool)
    context "String:" $ do
      it "Identity" $ property (prop_CustomSerializeIdentity putText getText :: T.Text -> Bool)
    context "EntityMetadata:" $ do
      it "Identity" $ property (prop_CustomSerializeIdentity putEntityMetadata getEntityMetadata :: EntityMetadata -> Bool)
    context "Slot:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: Slot -> Bool)
    context "Position:" $ do
      it "Identity" $ property (prop_CustomSerializeIdentity putPosition getPosition :: Position -> Bool)
    context "Angle:" $ do
      it "Identity" $ property (prop_CustomSerializeIdentity putAngle getAngle :: Angle -> Bool)
    context "UUID:" $ do
      it "Identity" $ property (prop_CustomSerializeIdentity putUUID getUUID :: UUID -> Bool)
    context "UUID':" $ do
      it "Identity" $ property (prop_CustomSerializeIdentity putUUID' getUUID' :: UUID -> Bool)
    context "ByteArray:" $ do
      it "Identity" $ property (prop_CustomSerializeIdentity putNetcodeByteString getNetcodeByteString :: B.ByteString -> Bool)
  describe "Minecraft Protocol Custom Records" $ do
    context "BlockChange:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: BlockChange -> Bool)
    context "Statistic:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: Statistic -> Bool)
    context "PlayerProperty:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: PlayerProperty -> Bool)
    context "PlayerListEntries:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: PlayerListEntries -> Bool)
    context "Icon:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: Icon -> Bool)
    context "EntityProperty:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: EntityProperty -> Bool)
    context "BlockAction:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: BlockAction -> Bool)
    context "BossBar:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: BossBarAction -> Bool)
    context "WorldBorderAction:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: WorldBorderAction -> Bool)
    context "UpdatedColumns:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: UpdatedColumns -> Bool)
    context "CombatEvent:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: CombatEvent -> Bool)
    context "ScoreboardMode:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: ScoreboardMode -> Bool)
    context "TeamMode:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: TeamMode -> Bool)
    context "TitleAction:" $ do
      it "Identity" $ property (prop_SerializeIdentity :: TitleAction -> Bool)

main :: IO ()
main = hspec spec
