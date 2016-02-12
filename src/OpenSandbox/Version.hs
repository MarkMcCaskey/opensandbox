{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Version
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Version
  ( openSandboxVersion
  , majorVersion
  , snapshotVersion
  , protocolVersion
  ) where

import qualified Data.Text as T

openSandboxVersion :: T.Text
openSandboxVersion = "0.0.1.1"


majorVersion :: T.Text
majorVersion = "1.9"


snapshotVersion :: T.Text
snapshotVersion = "16w06a"


protocolVersion :: Int
protocolVersion = 100
