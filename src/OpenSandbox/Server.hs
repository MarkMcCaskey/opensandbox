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
  ) where

import Control.Concurrent.STM.TVar
import OpenSandbox.Config
import OpenSandbox.Logger
import OpenSandbox.Time
import OpenSandbox.User
import OpenSandbox.World

data Server = Server
  { srvConfig :: Config
  , srvLogger :: Logger
  , srvWorldClock :: WorldClock
  , srvWorld :: World
  , srvUserStore :: TVar UserStore
  }
