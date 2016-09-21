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
import Generic.Random.Generic
import OpenSandbox
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Test
import OpenSandbox.World.ChunkSpec()
import Common
import Data.NBTSpec()

instance Arbitrary Chat where
  arbitrary = genericArbitrary

instance Arbitrary UpdatedColumns where
  arbitrary = do
    columns <- (arbitrary :: Gen Int8)
    if columns > 0
      then UpdatedColumns <$> return columns <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      else return NoUpdatedColumns

instance Arbitrary UpdateScoreAction where
  arbitrary = genericArbitrary

instance Arbitrary TitleAction where
  arbitrary = genericArbitrary

instance Arbitrary TeamMode where
  arbitrary = genericArbitrary

instance Arbitrary EntityProperty where
  arbitrary = genericArbitrary

instance Arbitrary WorldBorderAction where
  arbitrary = genericArbitrary

instance Arbitrary Icon where
  arbitrary = genericArbitrary

instance Arbitrary CombatEvent where
  arbitrary = genericArbitrary

instance Arbitrary EntityStatus where
  arbitrary = genericArbitrary

instance Arbitrary BlockChange where
  arbitrary = genericArbitrary

instance Arbitrary GameChangeReason where
  arbitrary = do
    i <- (choose (0,10) :: Gen Int)
    if i /= 9
      then return (toEnum i)
      else return (toEnum (i - 1))

instance Arbitrary Difficulty where
  arbitrary = genericArbitrary

instance Arbitrary GameMode where
  arbitrary = genericArbitrary

instance Arbitrary Dimension where
  arbitrary = genericArbitrary

instance Arbitrary WorldType where
  arbitrary = genericArbitrary

instance Arbitrary Statistic where
  arbitrary = genericArbitrary

instance Arbitrary ProtocolState where
  arbitrary = genericArbitrary

instance Arbitrary PlayerListEntries where
  arbitrary = genericArbitrary

instance Arbitrary PlayerListAdd where
  arbitrary = genericArbitrary

instance Arbitrary PlayerListUpdateGameMode where
  arbitrary = genericArbitrary

instance Arbitrary PlayerListUpdateLatency where
  arbitrary = genericArbitrary

instance Arbitrary PlayerListUpdateDisplayName where
  arbitrary = genericArbitrary

instance Arbitrary PlayerListRemovePlayer where
  arbitrary = genericArbitrary

instance Arbitrary SlotData where
  arbitrary = genericArbitrary

instance Arbitrary PlayerProperty where
  arbitrary = genericArbitrary

instance Arbitrary EntityMetadataEntry where
  arbitrary = do
    k <- arbitrary :: Gen Bool
    w <- choose (0,254) :: Gen Word8
    if k
      then return MetadataEnd
      else Entry <$> return w <*> arbitrary <*> arbitrary

instance Arbitrary MetadataType where
  arbitrary = genericArbitrary

instance Arbitrary Animation where
  arbitrary = genericArbitrary

instance Arbitrary UpdateBlockEntityAction where
  arbitrary = genericArbitrary

instance Arbitrary BlockAction where
  arbitrary = genericArbitrary

instance Arbitrary InstrumentType where
  arbitrary = genericArbitrary

instance Arbitrary NotePitch where
  arbitrary = genericArbitrary

instance Arbitrary PistonState where
  arbitrary = genericArbitrary

instance Arbitrary PistonDirection where
  arbitrary = genericArbitrary

instance Arbitrary BossBarAction where
  arbitrary = genericArbitrary

instance Arbitrary Slot where
  arbitrary = mkSlot <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UseEntityType where
  arbitrary = genericArbitrary

instance Arbitrary EntityHand where
  arbitrary = genericArbitrary

instance Arbitrary ScoreboardMode where
  arbitrary = genericArbitrary

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
