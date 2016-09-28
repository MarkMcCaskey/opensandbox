{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Handle.Handshaking
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Handle.Handshaking
  ( handleHandshaking
  , deserializeHandshaking
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.Attoparsec.ByteString as Decode
import qualified Data.ByteString as B
import Data.Conduit
import Data.Serialize
import qualified Data.Text as T
import OpenSandbox.Logger
import OpenSandbox.Protocol.Packet (SBHandshaking(..),SBStatus(..))
import OpenSandbox.Protocol.Types

logMsg :: Logger -> Lvl -> String -> IO ()
logMsg logger lvl msg = logIO logger "OpenSandbox.Protocol.Handle.Handshaking" lvl (T.pack msg)

handleHandshaking :: Logger -> Conduit (SBHandshaking,Maybe SBStatus) (StateT ProtocolState IO) SBStatus
handleHandshaking logger = awaitForever $ \handshake -> do
  liftIO $ logMsg logger LvlDebug $ "Recieving: " ++ show handshake
  case handshake of
    (SBHandshake _ _ _ ProtocolHandshake,_) -> do
      liftIO $ logMsg logger LvlDebug "Redundant handshake"
      return ()
    (SBHandshake _ _ _ ProtocolStatus,status) -> do
      liftIO $ logMsg logger LvlDebug "Switching protocol state to STATUS"
      lift $ State.put ProtocolStatus
      forM_ status yield
    (SBHandshake _ _ _ ProtocolLogin,_) -> do
      liftIO $ logMsg logger LvlDebug "Switching protocol state to LOGIN"
      lift $ State.put ProtocolLogin
      return ()
    (SBHandshake _ _ _ ProtocolPlay,_) -> do
      liftIO $ logMsg logger LvlDebug "Rejecting attempt to set protocol state to PLAY"
      return ()
    (SBLegacyServerListPing,_) -> do
      liftIO $ logMsg logger LvlDebug "Recieved LegacyServerListPing"
      return ()

deserializeHandshaking :: Conduit B.ByteString (StateT ProtocolState IO) (SBHandshaking,Maybe SBStatus)
deserializeHandshaking = do
    maybeBS <- await
    case maybeBS of
      Nothing -> return ()
      Just bs ->
        if B.take 2 bs /= "\254\SOH"
          then
            case runGet getSBHandshaking' bs of
              Left _ -> leftover bs
              Right (handshake,status) -> yield (handshake,status)
          else
            case Decode.parseOnly (Decode.takeByteString <* Decode.endOfInput) (B.tail bs) of
              Left _ -> leftover bs
              Right _ -> yield (SBLegacyServerListPing,Nothing)
  where
  getSBHandshaking' = do
    ln <- getVarInt
    bs <- getBytes ln
    case decode bs of
      Left err -> fail err
      Right handshake -> do
        end <- isEmpty
        if end
          then return (handshake,Nothing)
          else do
            ln' <- getVarInt
            earlyBs <- getBytes ln'
            case decode earlyBs of
              Left _ -> return (handshake,Nothing)
              Right earlyStatus -> return (handshake,Just earlyStatus)
