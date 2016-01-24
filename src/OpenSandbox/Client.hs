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
import            Control.Concurrent.STM

import            OpenSandbox.Protocol

type ClientName = B.ByteString

data ClientState = Status | Login | Play

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
