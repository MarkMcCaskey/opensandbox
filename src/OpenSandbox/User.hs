{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.User
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-- Basic bindings for loading and parsing the usercache.json
-- of a Minecraft server.
-------------------------------------------------------------------------------
module OpenSandbox.User (
    -- * Core User Types
      User (..)
    , Group (..)

    , buildUser
    -- * Loading & Saving usercache.json
    , readUserCache
    , writeUserCache
    ) where


import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString.Lazy as B
import            Data.Either
import qualified  Data.List as List
import qualified  Data.Map as Map
import            Data.Maybe
import qualified  Data.Text as T
import            Data.Time.Clock
import            Data.Time.Format
import            Data.Time.LocalTime
import            Data.UUID
import            Data.UUID.Aeson


-- | The core high-level type OpenSandbox uses for representing users.
data User = User
  { userUUID              :: !UUID
  , userName              :: !T.Text
  , userRealName          :: Maybe T.Text
  , userCacheExpiration   :: Maybe (UTCTime,TimeZone)
  } deriving (Show,Eq,Ord)


-- | Users can be organized into groups.
data Group = Group
  { groupID       :: Int
  , groupLabel    :: T.Text
  , groupUsers    :: [User]
  } deriving (Show,Eq,Ord)


nobody :: Group
nobody = Group 0 "nobody" []


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

buildUser :: Maybe UUID -> Maybe T.Text -> Maybe T.Text -> Maybe (UTCTime,TimeZone) -> Either String User
buildUser maybeUUID maybeUsername maybeRealname maybeCacheExp =
  if (isJust maybeUUID) && (isJust maybeUsername)
    then Right $ User (fromJust maybeUUID) (fromJust maybeUsername) maybeRealname maybeCacheExp
    else Left $ "Error: Invalid uuid or username"

-------------------------------------------------------------------------------

-- | Given a path for a valid usercache.json, this function will try to parse it
-- into a list of 'User's (no duplicates). Otherwise it will return an error.
readUserCache :: FilePath -> IO (Either String [User])
readUserCache path = do
    eitherUserEntries <- eitherDecode <$> B.readFile path
    case eitherUserEntries of
      Left err          -> return $ Left ("Error >> " ++ err)
      Right userEntries -> return $ Right (List.nub (fmap fromUserCacheEntry userEntries))


-- | Given a path for a new usercache.json, this function will convert a list
-- of 'User's to the original json representation used by usercache.json and
-- writes it out to disk.
writeUserCache :: FilePath -> [User] -> IO ()
writeUserCache path users = do
  let userCache = rights $ fmap toUserCacheEntry users
  let json = encode userCache
  B.writeFile path json


-- | Converts a low-level 'UserCacheEntry' type produced by decoding the json
-- is usercache.json into Open Sandbox's high-level User type.
fromUserCacheEntry :: UserCacheEntry -> User
fromUserCacheEntry u = User (uuid u) (name u) Nothing (Just $ mkTimeTuple $ parseCacheTime $ expiresOn u)
  where parseCacheTime x = parseTimeOrError False defaultTimeLocale "%F %T %z" x :: ZonedTime
        mkTimeTuple t = (zonedTimeToUTC t, zonedTimeZone t)


-- | Converts Open Sandbox's high-level 'User' type down into the low-level
-- 'UserCacheEntry' type, which can be easily encoded into JSON.
toUserCacheEntry :: User -> Either String UserCacheEntry
toUserCacheEntry u = do
    let isNotCacheable = isNothing $ userCacheExpiration u
    if isNotCacheable
      then Left "Error: Cannot cache User with no expiration date!"
      else Right $ UserCacheEntry (userName u) (userUUID u) (formatCacheTime $ mkZonedTime $ fromJust $ userCacheExpiration u)
  where formatCacheTime = formatTime defaultTimeLocale "%F %T %z"
        mkZonedTime (t,z) = utcToZonedTime z t
