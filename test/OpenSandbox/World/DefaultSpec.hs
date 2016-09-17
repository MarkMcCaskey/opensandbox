{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module OpenSandbox.World.DefaultSpec (main,spec) where

import Common
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
