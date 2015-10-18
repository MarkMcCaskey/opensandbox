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
module OpenSandbox.Minecraft.User (
    User (..),
    readUsers,
    writeUsers
) where


import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.UUID
import Data.UUID.Aeson
import GHC.Generics


data User = User
  { name      :: !T.Text
  , uuid      :: !UUID
  , expiresOn :: Maybe T.Text
  } deriving (Show,Eq,Read)


instance FromJSON User where
    parseJSON (Object v) =
        User <$> v .: "name"
             <*> v .: "uuid"
             <*> v .:? "expiresOn"
    parseJSON _ = mzero


instance ToJSON User where
    toJSON (User name uuid expiresOn) =
        object [ "name"         .= name
               , "uuid"         .= uuid
               , "expiresOn"    .= expiresOn
               ]


readUsers :: FilePath -> IO (Either String [User])
readUsers path = eitherDecode <$> B.readFile path


writeUsers :: FilePath -> [User] -> IO ()
writeUsers path users = do
    let json = encode users
    B.writeFile path json
