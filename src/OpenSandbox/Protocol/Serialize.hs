module OpenSandbox.Protocol.Serialize
  ( putVarInt
  , getVarInt
  ) where

import Data.Bits
import Data.Serialize

putVarInt :: Int -> Put
putVarInt i | i < 0x80 = putWord8 (fromIntegral i)
            | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> putVarInt (i `shiftR` 7)

getVarInt :: Get Int
getVarInt = do
    w <- getWord8
    if testBit w 7 then go 7 (fromIntegral (w .&. 0x7F))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- getWord8
      if testBit w' 7 then go (n+7) (val .|. ((fromIntegral (w' .&. 0x7F)) `shiftL` n))
        else return (val .|. ((fromIntegral w') `shiftL` n))
