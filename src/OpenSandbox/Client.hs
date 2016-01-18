-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Client
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable (requires Tmux)
--
-------------------------------------------------------------------------------
module OpenSandbox.Client where

import            Data.Conduit
import            Data.Conduit.Network
import            Data.Conduit.TMChan
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Char8 as BS
import            Control.Concurrent.STM

type ClientName = BS.ByteString

data Client = Client
  { clientName    :: ClientName
  , clientChan    :: TMChan B.ByteString
  , clientApp     :: AppData
  }

newClient :: ClientName -> AppData -> STM Client
newClient name app = do
  chan <- newTMChan
  return Client
    { clientName = name
    , clientApp  = app
    , clientChan = chan
    }
