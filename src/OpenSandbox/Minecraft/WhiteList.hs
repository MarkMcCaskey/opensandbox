{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.WhiteList
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-- A module intended for providing high-level bindings to the Minecraft's
-- whitelist.json, including:
--
--  * Loading whitelists
--  * Saving whitelists
--  * Reloading whitelists
--  * Enabling whitelists
--  * Disabling whitelists
--  * Adding users
--  * Removed users
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.WhiteList
    ( WhiteList
    , readWhiteList
    , writeWhiteList
    , enableWhiteList
    , disableWhiteList
    , addWhiteListUser
    , rmWhiteListUser
    ) where


import            Data.Aeson
import qualified  Data.ByteString.Lazy as B
import            Data.Set
import qualified  Data.Text as T
import            Data.UUID
import            Data.UUID.Aeson
import            GHC.Generics

import OpenSandbox.Minecraft.User
import OpenSandbox.Tmux


-- | A high-level data type that holds all whitelist information.
data WhiteList = WhiteList
  { whiteList       :: Set User
  , enabled         :: Bool
  , whiteListPath   :: FilePath
  } deriving (Show,Eq,Ord)


-- | A whitelist entry data type.
data WhiteListUser = WhiteListUser
  { uuid   :: !UUID
  , name   :: !T.Text
  } deriving (Show,Eq,Ord,Generic)


instance FromJSON WhiteListUser
instance ToJSON WhiteListUser


-- | Reads and decodes Minecraft's whitelist.json into a set of WhiteListUser
--  param: filepath of whitelist.json
--  return: Either a set of WhiteListUser or an error
readWhiteList :: FilePath -> IO (Either String (Set WhiteListUser))
readWhiteList path = eitherDecode <$> B.readFile path


-- | Encodes and writes a set of WhiteListUser out to disk as a json file
-- param: filepath of target being writen to
-- param: the set of WhiteListUser that needs to be encoded to json
--        and saved to disk
-- returns: An IO action that should produce the new json file
writeWhiteList :: FilePath -> Set WhiteListUser -> IO ()
writeWhiteList path whiteList = do
    let json = encode whiteList
    B.writeFile path json


buildWhiteListUser :: User -> WhiteListUser
buildWhiteListUser u = WhiteListUser (userUUID u) (userName u)



-- | Reloads Minecraft server's whitelist via a tmux command
-- param: The ID of the tmux session and window that the target server
--        resides in.
-- returns: An IO side effect that should reload the server's whitelist.
reloadWhiteList :: TmuxID -> IO ()
reloadWhiteList t = sendTmux t "whitelist reload"


-- | Enable a whitelist
-- param:   The whitelist to enable.
-- returns: A whitelist with the enabled flag set to true.
enableWhiteList :: WhiteList -> WhiteList
enableWhiteList w = WhiteList (whiteList w) True (whiteListPath w)


-- | Disable a whitelist
-- param:   The whitelist to disable.
-- returns: A whitelist with the enable flag set to false.
disableWhiteList :: WhiteList -> WhiteList
disableWhiteList w = WhiteList (whiteList w) False (whiteListPath w)


-- | Add a user to a whitelist
-- param:   The set of WhiteListUser, all unique users.
-- param:   The
-- returns: The new whitelist that includes the given user.
addWhiteListUser :: Set WhiteListUser -> WhiteListUser -> Set WhiteListUser
addWhiteListUser set user = insert user set


-- | Remove a user to a whitelist
-- param:   The set of WhiteListUser, all unique users.
-- param:   The user to remove from the whitelist.
-- returns: The new whitelist that excludes the given user.
rmWhiteListUser :: Set WhiteListUser -> WhiteListUser -> Set WhiteListUser
rmWhiteListUser set user = delete user set
