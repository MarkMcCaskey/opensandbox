{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Handle.Status
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Handle.Status
  ( handleStatus
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.Conduit
import qualified Data.Text as T
import OpenSandbox.Config
import OpenSandbox.Logger
import OpenSandbox.Protocol.Packet (CBStatus(..),SBStatus(..))
import OpenSandbox.Protocol.Types (ProtocolState(..))
import OpenSandbox.Version

logMsg :: Logger -> Lvl -> String -> IO ()
logMsg logger lvl msg = logIO logger "OpenSandbox.Protocol.Handle.Status" lvl (T.pack msg)

handleStatus  :: Config -> Logger -> Conduit SBStatus (StateT ProtocolState IO) CBStatus
handleStatus config logger = awaitForever $ \status ->
      case status of
        SBRequest -> sendAndLog $
          CBResponse
            snapshotVersion
            (toEnum protocolVersion)
            0
            (toEnum . fromEnum . srvMaxPlayers $ config)
            (srvMotd config)
        SBPing payload -> sendAndLog $ CBPong payload
  where
    sendAndLog :: MonadIO m => CBStatus -> Conduit SBStatus m CBStatus
    sendAndLog packet = do
      liftIO $ logMsg logger LvlDebug $ "Sending: " ++ show packet
      yield packet

