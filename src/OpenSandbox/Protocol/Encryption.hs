{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.Protocol.Encryption
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.Protocol.Encryption
  ( Encryption (..)
  , encrypt
  , decrypt
  , configEncryption
  ) where

import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Types hiding (End)
import Data.ByteString (ByteString,pack)
import qualified Data.ByteString as B
import Data.X509
import Data.Word
import Crypto.Cipher.AES (AES128)
import Crypto.PubKey.RSA
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),makeIV,nullIV)
import Crypto.Error (CryptoFailable(..))

data Encryption = Encryption
  { getCert         :: ByteString
  , getPubKey       :: PublicKey
  , getPrivKey      :: PrivateKey
  , getVerifyToken  :: VerifyToken
  } deriving (Show,Eq)

type VerifyToken = ByteString

data Key a = Key ByteString

encrypt :: ByteString -> ByteString -> ByteString
encrypt sharedSecret bs = cfbEncrypt ctx customIV bs
  where
    ctx = cipherInitNoErr (cipherMakeKey (undefined :: AES128) sharedSecret)
    customIV =
      case makeIV (updateIV sharedSecret (B.head bs)) of
        Nothing -> error "Something went wrong"
        Just iv -> iv
    cipherInitNoErr :: BlockCipher c => Key c -> c
    cipherInitNoErr (Key k) =
      case cipherInit k of
        CryptoPassed a -> a
        CryptoFailed e -> error (show e)
    cipherMakeKey :: Cipher cipher => cipher -> ByteString -> Key cipher
    cipherMakeKey _ = Key

decrypt :: ByteString -> ByteString -> ByteString
decrypt = encrypt

updateIV :: ByteString -> Word8 -> ByteString
updateIV iv dat = B.tail iv `B.snoc` dat

configEncryption :: IO Encryption
configEncryption = do
  (pubKey,privKey) <- generate 128 3645
  let cert = encodeASN1' DER $ toASN1 (PubKeyRSA pubKey) []
  return (Encryption cert pubKey privKey (pack [26,120,188,217]))
