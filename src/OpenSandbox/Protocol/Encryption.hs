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
  , encryptPacket
  , decryptPacket
  , encrypt
  , decrypt
  , configEncryption
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as State
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Types hiding (End)
import Data.ByteString (ByteString,pack)
import qualified Data.ByteString as B
import Data.Conduit
import Data.X509
import Data.Word
import Crypto.Cipher.AES (AES128)
import Crypto.PubKey.RSA
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),makeIV)
import Crypto.Error (CryptoFailable(..))

import OpenSandbox.Protocol.Types (Session (..))

data Encryption = Encryption
  { getCert         :: ByteString
  , getPubKey       :: PublicKey
  , getPrivKey      :: PrivateKey
  , getVerifyToken  :: VerifyToken
  } deriving (Show,Eq)

type VerifyToken = ByteString

data Key a = Key ByteString

encryptPacket :: Conduit B.ByteString (StateT Session IO) B.ByteString
encryptPacket = awaitForever $ \packet -> do
  s <- lift State.get
  if (sessionEncryptionIsActive s) && (sessionEncryptionIsEnabled s)
    then
      case sessionSharedSecret s of
        Nothing -> error "Could not get shared secret from session state!"
        Just sharedSecret -> yield (B.concat . fmap (encrypt sharedSecret) . breakup $ packet)
    else yield packet
  where
    breakup :: B.ByteString -> [B.ByteString]
    breakup bs = if B.length bs > 16
                 then (B.take 16 bs):breakup (B.drop 16 bs)
                 else [bs]

decryptPacket :: Conduit B.ByteString (StateT Session IO) B.ByteString
decryptPacket = awaitForever $ \packet -> do
  s <- lift State.get
  if (sessionEncryptionIsActive s) && (sessionEncryptionIsEnabled s)
    then
      case sessionSharedSecret s of
        Nothing -> error "Could not get shared secret from session state!"
        Just sharedSecret -> yield (B.concat . fmap (decrypt sharedSecret) . breakup $ packet)
    else yield packet
  where
    breakup :: B.ByteString -> [B.ByteString]
    breakup bs = if B.length bs > 16
                 then (B.take 16 bs):breakup (B.drop 16 bs)
                 else [bs]

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
