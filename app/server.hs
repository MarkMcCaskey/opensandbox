{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- File         : server.hs
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------


import            Control.Concurrent
import            Crypto.PubKey.RSA
import qualified  Data.Aeson as Aeson
import            Data.ASN1.BinaryEncoding
import            Data.ASN1.Encoding
import            Data.ASN1.BitArray
import            Data.ASN1.Types
import            Data.Bytes.VarInt
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import            Data.Maybe
import            Data.Serialize
import qualified  Data.Set as Set
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.UUID
import            Data.UUID.V4
import            Data.Word
import            Data.X509
import            GHC.Generics
import            Network.Socket hiding (send,recv)
import            Network.Socket.ByteString
import            OpenSandbox
import            OpenSandbox.Config
import            OpenSandbox.Minecraft.Protocol
import            OpenSandbox.Minecraft.User




mcVersion :: T.Text
mcVersion = "15w51b"


mcSrvPath :: FilePath
mcSrvPath = "."


data Encryption = Encryption
  { getCert         :: B.ByteString
  , getPubKey       :: PublicKey
  , getPrivKey      :: PrivateKey
  , getVerifyToken  :: B.ByteString
  } deriving (Show,Eq)

data Server = Server
  { srvName         :: T.Text
  , srvEncryption   :: Maybe Encryption
  , srvCompression  :: Maybe Compression
  , srvStatus       :: Status
  } deriving (Show,Eq)

configEncryption :: Config -> IO (Maybe Encryption)
configEncryption config =
  if mcOnlineMode config == True
    then do
      putStrLn "Encryption: [ENABLED]"
      putStrLn "Generating key pair"
      (pubKey,privKey) <- generate 128 65537
      let cert = encodeASN1' DER $ toASN1 (PubKeyRSA pubKey) []
      return (Just (Encryption cert pubKey privKey (B.pack [26,120,188,217])))
    else do
      putStrLn "Encryption: [DISABLED]"
      return Nothing


configCompression :: Config -> IO (Maybe Compression)
configCompression config =
  if mcNetworkCompressionThreshold config /= Nothing
    then do
      putStrLn "Compression: [ENABLED]"
      return (mcNetworkCompressionThreshold config)
    else do
      putStrLn "Compression: [DISABLED]" >> return Nothing


main :: IO ()
main = do
    let config = defaultConfig
    let port = 25567
    putStrLn "Welcome to OpenSandbox Server!"
    putStrLn "Loading OpenSandbox properties..."
    putStrLn $ "Starting minecraft server version " ++ show mcVersion
    putStrLn "Loading properties"
    loadMCServerProperties mcSrvPath
    putStrLn "Default game type: SURVIVAL"
    maybeEncryption <- configEncryption config
    maybeCompression <- configCompression config
    putStrLn $ "Starting Minecraft server on " ++ show port
    putStrLn $ "Preparing level " ++ show (mcLevelName config)
    putStrLn "Done!"
    let currentPlayers = 0
    let status = Status mcVersion currentPlayers (mcMaxPlayers config) (mcMotd config)
    let srv = Server "Opensandbox" maybeEncryption maybeCompression status
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet (toEnum port) iNADDR_ANY)
    listen sock 1
    mainLoop sock srv


loadMCServerProperties :: FilePath -> IO ()
loadMCServerProperties path = return ()


mainLoop :: Socket -> Server -> IO ()
mainLoop sock srv = do
    (conn,_) <- accept sock
    packet <- recv conn 256
    routeHandshake conn srv (decode packet :: Either String ServerBoundStatus)
    mainLoop sock srv


routeHandshake :: Socket -> Server -> Either String ServerBoundStatus -> IO ()
routeHandshake sock srv (Right (Handshake _ _ _ 1)) = runStatus sock srv
routeHandshake sock srv (Right (Handshake _ _ _ 2)) = runLogin sock srv
routeHandshake sock srv (Right (Handshake _ _ _ _)) = putStrLn "Error: Unknown state!"
routeHandshake sock srv (Left _)                    = putStrLn "Error: Unknown packet"


runStatus :: Socket -> Server -> IO ()
runStatus sock srv = do
    putStrLn "================================================================="
    putStrLn "|                   << Packet Report Begin >>                   |"
    putStrLn "================================================================="
    let response = BL.toStrict $ Aeson.encode $ buildStatus (srvStatus srv)
    let response' = B.cons (0 :: Word8) (B.cons (fromIntegral $ B.length response :: Word8) response)
    let outgoing = B.cons (fromIntegral $ B.length response' :: Word8) response'
    send sock outgoing
    mapM_ putStrLn bar
    ping <- recv sock 256
    putStrLn "[Raw Ping]"
    print ping
    print $ B.unpack ping
    maybePing sock ping
    mapM_ putStrLn bar
    putStrLn "|                    << Packet Report End >>                    |"
    putStrLn "================================================================="
    sClose sock


runLogin :: Socket -> Server -> IO ()
runLogin sock srv = do
    loginStart <- recv sock 254
    let someUsername = decodeUtf8 $ B.drop 3 loginStart
    someUUID <- nextRandom
    let someUser = User someUUID someUsername Nothing Nothing
    print someUser
    send sock (loginSuccess someUser)
    runPlay sock srv
    sClose sock


runPlay :: Socket -> Server -> IO ()
runPlay sock srv = undefined


maybePing :: Socket -> B.ByteString -> IO ()
maybePing sock maybePing =
    if (B.length maybePing == 10) && (B.index maybePing 1 == 1)
        then do
              send sock maybePing
              return ()
        else do
              packet <- recv sock 254
              send sock packet
              return ()


bar :: [String]
bar = [ "================================================================="
      , "/////////////////////////////////////////////////////////////////"
      , "================================================================="]
