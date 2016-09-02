module OpenSandbox.Protocol.TypesSpec (main,spec) where
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
import CommonSpec()
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
  arbitrary = fmap toEnum (choose (0,9) :: Gen Int)

instance Arbitrary ChunkSection where
  arbitrary = do
    m <- arbitrary :: Gen Bool
    ChunkSection
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (fmap B.pack $ vectorOf 2048 (arbitrary :: Gen Word8))
      <*> case m of
          False -> return Nothing
          True -> fmap Just (fmap B.pack $ vectorOf 2048 (arbitrary :: Gen Word8))

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

prop_varIntEq :: [VarInt] -> Bool
prop_varIntEq [] = True
prop_varIntEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeVarInt x)) lst
  let decoded = fmap (Decode.parseOnly decodeVarInt) encoded :: [Either String VarInt]
  lst == (rights decoded)

prop_varLongEq :: [VarLong] -> Bool
prop_varLongEq [] = True
prop_varLongEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeVarLong x)) lst
  let decoded = fmap (Decode.parseOnly decodeVarLong) encoded :: [Either String VarLong]
  lst == (rights decoded)

prop_textEq :: [T.Text] -> Bool
prop_textEq [] = True
prop_textEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeText x)) lst
  let decoded = fmap (Decode.parseOnly decodeText) encoded :: [Either String T.Text]
  lst == (rights decoded)

prop_entityMetadataEq :: [EntityMetadata] -> Bool
prop_entityMetadataEq [] = True
prop_entityMetadataEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeEntityMetadata x)) lst
  let decoded = fmap (Decode.parseOnly decodeEntityMetadata) encoded :: [Either String EntityMetadata]
  lst == (rights decoded)

prop_slotEq :: [Slot] -> Bool
prop_slotEq [] = True
prop_slotEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeSlot x)) lst
  let decoded = fmap (Decode.parseOnly decodeSlot) encoded :: [Either String Slot]
  lst == (rights decoded)

prop_chunkSectionEq :: [ChunkSection] -> Bool
prop_chunkSectionEq [] = True
prop_chunkSectionEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeChunkSection x)) lst
  let decoded = fmap (Decode.parseOnly decodeChunkSection) encoded :: [Either String ChunkSection]
  lst == (rights decoded)

prop_positionEq :: [Position] -> Bool
prop_positionEq [] = True
prop_positionEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodePosition x)) lst
  let decoded = fmap (Decode.parseOnly decodePosition) encoded :: [Either String Position]
  lst == (rights decoded)

prop_angleEq :: [Angle] -> Bool
prop_angleEq [] = True
prop_angleEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeAngle x)) lst
  let decoded = fmap (Decode.parseOnly decodeAngle) encoded :: [Either String Angle]
  lst == (rights decoded)

prop_uuidEq :: [UUID] -> Bool
prop_uuidEq [] = True
prop_uuidEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeUUID x)) lst
  let decoded = fmap (Decode.parseOnly decodeUUID) encoded :: [Either String UUID]
  lst == (rights decoded)

prop_byteStringEq :: [B.ByteString] -> Bool
prop_byteStringEq [] = True
prop_byteStringEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeByteString x)) lst
  let decoded = fmap (Decode.parseOnly decodeByteString) encoded :: [Either String B.ByteString]
  lst == (rights decoded)

prop_statisticEq :: [Statistic] -> Bool
prop_statisticEq [] = True
prop_statisticEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeStatistic x)) lst
  let decoded = fmap (Decode.parseOnly decodeStatistic) encoded :: [Either String Statistic]
  lst == (rights decoded)

prop_playerPropertyEq :: [PlayerProperty] -> Bool
prop_playerPropertyEq [] = True
prop_playerPropertyEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodePlayerProperty x)) lst
  let decoded = fmap (Decode.parseOnly decodePlayerProperty) encoded :: [Either String PlayerProperty]
  lst == (rights decoded)

prop_playerListEntriesEq :: [PlayerListEntries] -> Bool
prop_playerListEntriesEq [] = True
prop_playerListEntriesEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodePlayerListEntries x)) lst
  let decoded = fmap (Decode.parseOnly decodePlayerListEntries) encoded :: [Either String PlayerListEntries]
  lst == (rights decoded)

prop_iconEq :: [Icon] -> Bool
prop_iconEq [] = True
prop_iconEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeIcon x)) lst
  let decoded = fmap (Decode.parseOnly decodeIcon) encoded :: [Either String Icon]
  lst == (rights decoded)

prop_entityPropertyEq :: [EntityProperty] -> Bool
prop_entityPropertyEq [] = True
prop_entityPropertyEq lst = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (encodeEntityProperty x)) lst
  let decoded = fmap (Decode.parseOnly decodeEntityProperty) encoded :: [Either String EntityProperty]
  lst == (rights decoded)

{-
prop_ChunkSectionFieldEq :: ChunkSection -> Bool
prop_ChunkSectionFieldEq dat = do
  let encoded = BL.toStrict . Encode.toLazyByteString
                  $ (encodeVarInt ln) <> bs
  let decoded = Decode.parseOnly parser' encoded :: Either String ChunkSection
  [dat] == (rights [decoded])
  where
  bs = V.foldl' (<>) mempty $ fmap encodeChunkSection [dat]
  ln = B.length . BL.toStrict . Encode.toLazyByteString $ bs
  parser' :: Decode.Parser ChunkSection
  parser' = do
    ln' <- decodeVarInt
    bs' <- Decode.take ln'
    let result = Decode.parseOnly decodeChunkSection bs' :: Either String ChunkSection
    case result of
      Left err -> fail err
      Right result' -> return result'
-}

spec :: Spec
spec = do
  describe "Minecraft Protocol Core Types" $ do
    context "VarInt:" $ do
      it "Identity" $ property prop_varIntEq
    context "VarLong:" $ do
      it "Identity" $ property prop_varLongEq
    context "String:" $ do
      it "Identity" $ property prop_textEq
    context "EntityMetadata:" $ do
      it "Identity" $ property prop_entityMetadataEq
    context "Slot:" $ do
      it "Identity" $ property prop_slotEq
    context "ChunkSection:" $ do
      it "Identity" $ property prop_chunkSectionEq
    context "Position:" $ do
      it "Identity" $ property prop_positionEq
    context "Angle:" $ do
      it "Identity" $ property prop_angleEq
    context "UUID:" $ do
      it "Identity" $ property prop_uuidEq
    context "ByteArray:" $ do
      it "Identity" $ property prop_byteStringEq

  describe "Minecraft Protocol Custom Records" $ do
    context "Statistic:" $ do
      it "Identity" $ property prop_statisticEq
    context "PlayerProperty:" $ do
      it "Identity" $ property prop_playerPropertyEq
    context "PlayerListEntries:" $ do
      it "Identity" $ property prop_playerListEntriesEq
    context "Icon:" $ do
      it "Identity" $ property prop_iconEq
    context "EntityProperty:" $ do
      it "Identity" $ property prop_entityPropertyEq
    --context "ChunkSectionField:" $ do
    --  it "Identity" $ property prop_ChunkSectionFieldEq

main :: IO ()
main = hspec spec
