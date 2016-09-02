module OpenSandbox.Protocol.Serialize
  ( putVarInt
  , getVarInt
  , putVarLong
  , getVarLong
  ) where

import Data.Bits
import Data.Int
import Data.Serialize

putVarInt :: Int -> Put
putVarInt i
  | i < 0     = putVarInt (abs i + (2^31 :: Int))
  | i < 0x80  = putWord8 (fromIntegral i)
  | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> putVarInt (i `shiftR` 7)

getVarInt :: Get Int
getVarInt = do
    w <- getWord8
    if testBit w 7
      then do
        result <- go 7 (fromIntegral (w .&. 0x7F))
        if result <= (2^31)
          then return result
          else return (negate (result - (2^31)))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- getWord8
      if testBit w' 7
        then go (n+7) (val .|. ((fromIntegral (w' .&. 0x7F)) `shiftL` n))
        else return (val .|. ((fromIntegral w') `shiftL` n))

putVarLong :: Int64 -> Put
putVarLong i
  | i < 0     = putVarLong (abs i + (2^62 :: Int64))
  | i < 0x80  = putWord8 (fromIntegral i)
  | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >>  putVarLong (i `shiftR` 7)

getVarLong :: Get Int64
getVarLong = do
    w <- getWord8
    if testBit w 7
      then do
        result <- go 7 (fromIntegral (w .&. 0x7F))
        if result <= (2^62)
          then return result
          else return (negate (result - (2^62 :: Int64)))
      else return (fromIntegral w)
  where
    go n val = do
      w' <- getWord8
      if testBit w' 7
        then go (n+7) (val .|. (fromIntegral (w' .&. 0x7F) `shiftL` n))
        else return (val .|. (fromIntegral w' `shiftL` n))
