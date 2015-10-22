{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module:      : OpenSandbox.Minecraft.BannedIP
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Minecraft.BannedIP
    ( BannedIP
    , readBannedIPs
    , writeBannedIPs
    ) where


import            Control.Applicative
import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString.Lazy as B
import            Data.IP (IPv4 (..))
import            Data.Set
import qualified  Data.Text as T
import            GHC.Generics


data BannedIP = BannedIP
  { ip       :: !IPv4
  , created  :: !T.Text
  , source   :: !T.Text
  , expires  :: !T.Text
  , reason   :: !T.Text
  } deriving (Show,Eq,Ord,Generic)


instance FromJSON IPv4 where
    parseJSON (String ip) = pure (read $ T.unpack ip :: IPv4)
    parseJSON _ = mzero


instance ToJSON IPv4 where
    toJSON = String . T.pack . show


instance FromJSON BannedIP
instance ToJSON BannedIP


readBannedIPs :: FilePath -> IO (Either String (Set BannedIP))
readBannedIPs path = eitherDecode <$> B.readFile path


writeBannedIPs :: FilePath -> Set BannedIP -> IO ()
writeBannedIPs path bannedIPs = do
    let json = encode bannedIPs
    B.writeFile path json
