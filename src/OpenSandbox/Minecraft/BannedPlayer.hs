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
module OpenSandbox.Minecraft.BannedPlayer (
    BannedPlayer,
    readBannedPlayers,
    writeBannedPlayers
) where

-- External Imports
import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Set
import qualified Data.Text as T
import Data.UUID
import Data.UUID.Aeson
import GHC.Generics


data BannedPlayer = BannedPlayer
  { uuid     :: !UUID
  , name     :: !T.Text
  , created  :: !T.Text
  , source   :: !T.Text
  , expires  :: !T.Text
  , reason   :: !T.Text
  } deriving (Show,Eq,Ord,Generic)


instance FromJSON BannedPlayer
instance ToJSON BannedPlayer


readBannedPlayers :: FilePath -> IO (Either String (Set BannedPlayer))
readBannedPlayers path = eitherDecode <$> B.readFile path


writeBannedPlayers :: FilePath -> Set BannedPlayer -> IO ()
writeBannedPlayers path bannedPlayers = do
    let json = encode bannedPlayers
    B.writeFile path json
