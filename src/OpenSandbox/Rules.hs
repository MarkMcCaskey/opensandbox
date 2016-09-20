module OpenSandbox.Rules
  ( DiggingStatus (..)
  ) where

import Data.Serialize
import OpenSandbox.Protocol.Types (putVarInt,getVarInt)

data DiggingStatus
  = StartedDigging
  | CancelledDigging
  | FinishedDigging
  | DropItemStack
  | DropItem
  | ShootArrowOrFinishEating
  | SwapItemInHand
  deriving (Show,Eq,Enum)

instance Serialize DiggingStatus where
  put = putVarInt . fromEnum
  get = toEnum <$> getVarInt
