{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Minecraft.User
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-- Basic bindings for loading and parsing the usercache.json
-- of a Minecraft server.
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.User (
    -- * Core User Types
      User (..)
    , UserGroup

    -- * User Management
    , createUserGroup

    -- * Loading & Saving usercache.json
    , readUserCache
    , writeUserCache
    ) where


import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString.Lazy as B
import            Data.Either
import            Data.List
import            Data.Maybe
import            Data.Set
import qualified  Data.Text as T
import            Data.Time.Clock
import            Data.Time.Format
import            Data.Time.LocalTime
import            Data.UUID
import            Data.UUID.Aeson


-- | The core high-level type OpenSandbox uses for representing users.
data User = User
  { userName              :: !T.Text
  , userUUID              :: !UUID
  , userRealName          :: Maybe T.Text
  , userCacheExpiration   :: Maybe (UTCTime,TimeZone)
  } deriving (Show,Eq,Ord)


-- | Users can be organized into groups.
type UserGroup = Set User


-- | An internal type used for representing entries in usercache.json as is.
-- The fields in this type are then used to build a 'User' type.
data UserCacheEntry = UserCacheEntry
  { name      :: !T.Text
  , uuid      :: !UUID
  , expiresOn :: !String
  } deriving (Show,Eq)


instance FromJSON UserCacheEntry where
    parseJSON (Object v) =
        UserCacheEntry <$> v .: "name"
                       <*> v .: "uuid"
                       <*> v .: "expiresOn"
    parseJSON _ = mzero


instance ToJSON UserCacheEntry where
    toJSON (UserCacheEntry name uuid expiresOn) =
        object [ "name"         .= name
               , "uuid"         .= uuid
               , "expiresOn"    .= expiresOn
               ]


-- | Takes a list of users, sorts it, and then converts it to a 'UserGroup',
-- which is just a set of users.
createUserGroup :: [User] -> UserGroup
createUserGroup [] = empty
createUserGroup [x] = singleton x
createUserGroup u = fromDistinctAscList $ sort u


-- | Given a path for a valid usercache.json, this function will try to parse it
-- into a list of 'User's (no duplicates). Otherwise it will return an error.
readUserCache :: FilePath -> IO (Either String [User])
readUserCache path = do
    eitherUserEntries <- eitherDecode <$> B.readFile path
    case eitherUserEntries of
      Left err          -> return $ Left $ "Error >> " ++ err
      Right userEntries -> return $ Right (nub (fmap buildUser userEntries))


-- | Given a path for a new usercache.json, this function will convert a list
-- of 'User's to the original json representation used by usercache.json and
-- writes it out to disk.
writeUserCache :: FilePath -> [User] -> IO ()
writeUserCache path users = do
  let userCache = rights $ fmap buildUserCache users
  let json = encode userCache
  B.writeFile path json


-- | Converts a low-level 'UserCacheEntry' type produced by decoding the json
-- is usercache.json into Open Sandbox's high-level User type.
buildUser :: UserCacheEntry -> User
buildUser u = User (name u) (uuid u) Nothing (Just $ mkTimeTuple $ parseCacheTime $ expiresOn u)
  where parseCacheTime x = parseTimeOrError False defaultTimeLocale "%F %T %z" x :: ZonedTime
        mkTimeTuple t = (zonedTimeToUTC t, zonedTimeZone t)


-- | Converts Open Sandbox's high-level 'User' type down into the low-level
-- 'UserCacheEntry' type, which can be easily encoded into JSON.
buildUserCache :: User -> Either String UserCacheEntry
buildUserCache u = do
    let isNotCacheable = isNothing $ userCacheExpiration u
    if isNotCacheable
      then Left "Error: Cannot cache User with no expiration date!"
      else Right $ UserCacheEntry (userName u) (userUUID u) (formatCacheTime $ mkZonedTime $ fromJust $ userCacheExpiration u)
  where formatCacheTime = formatTime defaultTimeLocale "%F %T %z"
        mkZonedTime (t,z) = utcToZonedTime z t
