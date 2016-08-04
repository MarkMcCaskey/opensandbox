{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Data.Yggdrasil
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Data.Yggdrasil
  ( AuthenticatePayload
  , AuthenticateResponse
  , RefreshPayload
  , RefreshResponse
  , ValidatePayload
  , SignoutPayload
  , InvalidatePayload
  , Agent
  , AvailableProfile
  , SelectedProfile
  , YggdrasilResponseError
  , YggdrasilError
  ) where

import            Data.Aeson
import            Data.Aeson.Types
import qualified  Data.Text as T
import            Data.Word
import            GHC.Generics (Generic)

data AuthenticatePayload = AuthenticatePayload
  { agent         :: Agent
  , username      :: T.Text
  , password      :: T.Text
  , clientToken   :: Maybe T.Text
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data AuthenticateResponse = AuthenticateResponse
  { accessToken         :: T.Text
  , clientToken         :: T.Text
  , availableProfiles   :: Maybe [AvailableProfile]
  , selectedProfile     :: Maybe SelectedProfile
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data RefreshPayload = RefreshPayload
  { accessToken       :: T.Text
  , clientToken       :: T.Text
  , selectedProfile   :: SelectedProfile
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data RefreshResponse = RefreshResponse
  { accessToken       :: T.Text
  , clientToken       :: T.Text
  , selectedProfile   :: SelectedProfile
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data ValidatePayload = ValidatePayload
  { accessToken :: T.Text
  , clientToken :: T.Text
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data SignoutPayload = SignoutPayload
  { username :: T.Text
  , password :: T.Text
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data InvalidatePayload = InvalidatePayload
  { accessToken :: T.Text
  , clientToken :: T.Text
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data Agent = Agent
  { name      :: T.Text
  , version   :: Word8
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data AvailableProfile = AvailableProfile
  { id      :: T.Text
  , name    :: T.Text
  , legacy  :: Maybe Bool
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data SelectedProfile = SelectedProfile
  { id      :: T.Text
  , name    :: T.Text
  , legacy  :: Maybe Bool
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data YggdrasilResponseError = YggdrasilResponseError
  { error         :: YggdrasilError
  , errorMessage  :: T.Text
  , cause         :: Maybe T.Text
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data YggdrasilError
  = MethodNotAllowed
  | NotFound
  | ForbiddenOperationException
  | IllegalArgumentException
  | UnsupportedMediaType
  deriving (Show,Eq)

instance ToJSON YggdrasilError where
  toJSON ye =
    case ye of
      MethodNotAllowed            -> String "Method Not Allowed"
      NotFound                    -> String "Not Found"
      ForbiddenOperationException -> String "ForbiddenOperationException"
      IllegalArgumentException    -> String "IllegalArgumentException"
      UnsupportedMediaType        -> String "Unsupported Media Type"

instance FromJSON YggdrasilError where
  parseJSON (String s) =
    case s of
      "Method Not Allowed"            -> return MethodNotAllowed
      "Not Found"                     -> return NotFound
      "ForbiddenOperationException"   -> return ForbiddenOperationException
      "IllegalArgumentException"      -> return IllegalArgumentException
      "Unsupported Media Type"        -> return UnsupportedMediaType

  parseJSON x = typeMismatch "Unknown Yggdrasil error!" x
