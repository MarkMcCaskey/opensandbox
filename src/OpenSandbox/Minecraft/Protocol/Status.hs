module OpenSandbox.Minecraft.Protocol.Status
  ( Response (..)
  , Pong (..)
  , Request (..)
  , Ping (..)
  ) where

data Response = Response deriving (Show,Eq,Read)
data Pong = Pong deriving (Show,Eq,Read)
data Request = Request deriving (Show,Eq,Read)
data Ping = Ping deriving (Show,Eq,Read)
