{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Copyright        : (c) 2015 Michael Carpenter
-- License          : BSD3
-- Maintainer       : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability        : provisional
-- Portability      : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.WhiteList (
    WhiteListUser,
    readWhiteList,
    writeWhiteList,
    addWhiteListUser,
    rmWhiteListUser
) where


import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Set
import qualified Data.Text as T
import Data.UUID
import Data.UUID.Aeson
import GHC.Generics


data WhiteListUser = WhiteListUser
  { uuid   :: !UUID
  , name   :: !T.Text
  } deriving (Show,Eq,Ord,Generic)


instance FromJSON WhiteListUser
instance ToJSON WhiteListUser


readWhiteList :: FilePath -> IO (Either String (Set WhiteListUser))
readWhiteList path = eitherDecode <$> B.readFile path


writeWhiteList :: FilePath -> Set WhiteListUser -> IO ()
writeWhiteList path whiteList = do
    let json = encode whiteList
    B.writeFile path json


addWhiteListUser :: Set WhiteListUser -> WhiteListUser -> Set WhiteListUser
addWhiteListUser set user = insert user set


rmWhiteListUser :: Set WhiteListUser -> WhiteListUser -> Set WhiteListUser
rmWhiteListUser set user = delete user set
