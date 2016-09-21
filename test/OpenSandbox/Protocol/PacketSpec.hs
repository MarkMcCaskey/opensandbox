{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module OpenSandbox.Protocol.PacketSpec (main,spec) where

import Control.Monad
import Generic.Random.Generic
import OpenSandbox
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Test
import Common
import OpenSandbox.World.ChunkSpec()
import OpenSandbox.Protocol.TypesSpec()

instance Arbitrary SBHandshaking where
  arbitrary = genericArbitrary

instance Arbitrary SBStatus where
  arbitrary = genericArbitrary

instance Arbitrary CBStatus where
  arbitrary = genericArbitrary

instance Arbitrary CBLogin where
  arbitrary = genericArbitrary

instance Arbitrary SBLogin where
  arbitrary = genericArbitrary

instance Arbitrary CBPlay where
  arbitrary = genericArbitrary

instance Arbitrary SBPlay where
  arbitrary = genericArbitrary

spec :: Spec
spec =
  describe "Minecraft Protocol Packets" $ do
    context "Server bound handshaking packets:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: SBHandshaking -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Client bound status packets:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: CBStatus -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Server bound status packets:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: SBStatus -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Client bound login packets:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: CBLogin -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Server bound login packets:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: SBLogin -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Client bound play packets:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: CBPlay -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True
    context "Server bound play packets:" $
      it "Identity" $ do
        r <- quickCheckWithResult
          stdArgs
          (prop_SerializeIdentity :: SBPlay -> Bool)
        unless (isSuccess r) $ print r
        isSuccess r `shouldBe` True

main :: IO ()
main = hspec spec
