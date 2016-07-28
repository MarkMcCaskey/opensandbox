{-# LANGUAGE OverloadedLists #-}
module OpenSandbox.World
  ( genFlatWorld
  ) where

import            Data.Bits
import qualified  Data.ByteString as B
import            Data.Int
import qualified  Data.Vector as V
import            Data.Word
import            OpenSandbox.Data.Protocol
import            OpenSandbox.Data.Block

genFlatWorld :: Int32 -> [CBPlay]
genFlatWorld radius = [chunkDataPacket1 x z | x <- [(-radius)..radius], z <- [(-radius)..radius]]
  where
  chunkDataPacket1 x z = CBChunkData x z 1 chunkSections1 (Just $ B.replicate 256 1) V.empty
  chunkSections1 = OverWorldChunkSections [chunkSection1]
  chunkSection1 = OverWorldChunkSection 4 [0,112,48,32] flatWorldBase (B.replicate 2048 0) (B.replicate 2048 255)
  flatWorldBase = V.concat
    [ bedrockLayer
    , dirtLayer
    , dirtLayer
    , grassLayer
    , (V.concat $ replicate 12 airLayer)
    ]
  airLayer = V.replicate 16 (0 :: Int64)
  grassLayer = V.replicate 16 $ V.foldl' (xor) 0 $
    V.zipWith setBit
      (V.replicate 32 (0 :: Int64))
      [0,1,4,5,8,9,12,13,16,17,20,21,24,25,28,29,32,33,36,37,40,41,44,45,48,49,52,53,56,57,60,61]
  dirtLayer = V.replicate 16 $ V.foldl' (xor) 0 $
    V.zipWith setBit
      (V.replicate 16 (0 :: Int64))
      [1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61]
  bedrockLayer = V.replicate 16 $ V.foldl' (xor) 0 $
    V.zipWith setBit
      (V.replicate 16 (0 :: Int64))
      [0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60]
