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

import Data.Text

openSandboxVersion :: Text
openSandboxVersion = "0.0.1.4"


majorVersion :: Text
majorVersion = "1.10"


snapshotVersion :: Text
snapshotVersion = "1.10"


protocolVersion :: Int
protocolVersion = 210
