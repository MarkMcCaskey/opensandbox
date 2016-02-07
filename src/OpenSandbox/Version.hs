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


openSandboxVersion :: String
openSandboxVersion = "0.0.1.1"


majorVersion :: String
majorVersion = "1.9"


snapshotVersion :: String
snapshotVersion = "16w05b"


protocolVersion :: Int
protocolVersion = 99
