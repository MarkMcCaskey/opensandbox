{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
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
  ( ClientAuth
  , ServerAuth
  , authDigest
  , twosComplement
  , AuthenticatePayload
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

import            Crypto.Hash
import            Data.Aeson
import            Data.Aeson.Types
import            Data.Bits
import            Data.ByteArray
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as Encode
import            Data.Monoid
import qualified  Data.Text as T
import            Data.Word
import            GHC.Generics (Generic)

data ClientAuth = ClientAuth
  { accessToken       :: T.Text
  , selectedProfile   :: SelectedProfile
  , serverId          :: T.Text
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data ServerAuth = ServerAuth
  { id          :: T.Text
  , name        :: T.Text
  , properties  :: [AuthProperty]
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

data AuthProperty = AuthProperty
  { name        :: T.Text
  , value       :: T.Text
  , signature   :: T.Text
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

authDigest :: B.ByteString -> B.ByteString
authDigest bs = do
  let bs' = B.pack . unpack . hashWith SHA1 $ bs
  if (B.index bs' 0 .&. 0x80) == 0x80
    then B.dropWhile (==0) . twosComplement $ bs'
    else B.dropWhile (==0) bs'

twosComplement :: B.ByteString -> B.ByteString
twosComplement bs = BL.toStrict $ Encode.toLazyByteString (go (B.length bs - 1) True mempty)
  where
  go :: Int -> Bool -> Encode.Builder -> Encode.Builder
  go (-1) _ bs' = bs'
  go i carry bs' = do
    let !b = B.index bs i
    let !b' = complement b .&. 0xff
    if carry
      then go (i - 1) (b' == 0xff) (Encode.word8 (b + 1) <> bs')
      else go (i - 1) carry (Encode.word8 b <> bs')


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
      _                               -> undefined

  parseJSON x = typeMismatch "Unknown Yggdrasil error!" x
