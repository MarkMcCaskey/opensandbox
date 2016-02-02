-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Server
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Server
  ( Server (..)
  , Servers
  ) where

import qualified  Data.Map as Map
import            OpenSandbox.Config

type Servers = Map.Map String Server

data Server = Server
  { srvName         :: String
  , srvPort         :: Int
  , srvPath         :: FilePath
  , srvBackupPath   :: FilePath
  , srvLogPath      :: FilePath
  , srvWorld        :: String
  , srvVersion      :: String
  , srvPlayers      :: Int
  , srvMaxPlayers   :: Int
  , srvMotd         :: String
  , srvEncryption   :: Maybe Encryption
  , srvCompression  :: Maybe Compression
  , srvEnabled      :: Bool
  , srvUp           :: Bool
  } deriving (Show,Eq)
