import OpenSandbox
import OpenSandbox.Minecraft.Protocol.Handshaking
import Data.Binary
import Test.QuickCheck


--prop_handshakeSerialize :: Yggdrasil -> Bool
--prop_handshakeSerialize y = ((decode . encode) y :: Yggdrasil) == id y


main :: IO ()
main = putStrLn "Tests not implemented yet."

    --quickCheck prop_handshakeSerialize
