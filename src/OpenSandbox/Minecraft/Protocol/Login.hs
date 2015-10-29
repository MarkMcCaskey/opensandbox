-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.Protocol.Login
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.Protocol.Login
  ( ClientBoundLogin
  , ServerBoundLogin
  ) where


import qualified  Data.ByteString as B
import qualified  Data.Text as T
import            Data.Word

data ClientBoundLogin
  = ClientBoundDisconnect B.ByteString
  | ClientBoundEncryptionRequest T.Text B.ByteString B.ByteString
  | ClientBoundLoginSuccess T.Text T.Text
  | ClientBoundSetCompression Word16
  deriving (Show,Eq)


data ServerBoundLogin
  = ServerBoundLoginStart T.Text
  | ServerBoundEncryptionResponse B.ByteString B.ByteString
  deriving (Show,Eq)
