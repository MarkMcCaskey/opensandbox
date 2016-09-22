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
import Data.X509
import Crypto.Cipher.AES (AES128)
import Crypto.PubKey.RSA
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),makeIV)
import Crypto.Error (CryptoFailable(..))

data Encryption = Encryption
  { getCert         :: ByteString
  , getPubKey       :: PublicKey
  , getPrivKey      :: PrivateKey
  , getVerifyToken  :: VerifyToken
  } deriving (Show,Eq)

type SharedSecret = ByteString
type VerifyToken = ByteString

data Key a = Key ByteString

encrypt :: ByteString -> ByteString -> ByteString
encrypt sharedSecret = ctrCombine ctx customIV
  where
    ctx = cipherInitNoErr (cipherMakeKey (undefined :: AES128) sharedSecret)
    customIV =
      case makeIV sharedSecret of
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

configEncryption :: IO Encryption
configEncryption = do
  (pubKey,privKey) <- generate 128 3645
  let cert = encodeASN1' DER $ toASN1 (PubKeyRSA pubKey) []
  return (Encryption cert pubKey privKey (pack [26,120,188,217]))
