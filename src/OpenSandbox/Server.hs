-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Server
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable (requires Tmux)
--
-------------------------------------------------------------------------------
module OpenSandbox.Server
  ( Server (..)
  , Servers
  ) where

import qualified  Data.Map as M
import            Network.Socket
import            OpenSandbox.Config

type Servers = M.Map String Server


data Server = Server
  { srvName         :: String
  , srvPort         :: PortNumber
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

