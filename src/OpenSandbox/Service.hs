-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Service
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable (requires Tmux)
--
-------------------------------------------------------------------------------
module OpenSandbox.Service
  ( Service (..)
  , Services
  , ServiceName
  ) where

import qualified  Data.Map as Map


type Services = Map.Map ServiceName Service


type ServiceName = String


data Service = Service
  { srvName       :: !ServiceName
  , srvPort       :: !Int
  , srvPath       :: !FilePath
  , srvBackupPath :: !FilePath
  , srvLogPath    :: !FilePath
  , srvWorld      :: !String
  , srvVersion    :: !String
  } deriving (Show,Eq,Ord)


