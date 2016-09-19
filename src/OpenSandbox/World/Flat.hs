{-# LANGUAGE OverloadedLists #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.World.Flat
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.World.Flat
  ( genFlatWorld
  , ChunkLayers
  , mkChunkLayers
  , flatChunkColumn
  , classicFlatPreset
  ) where

import Data.Int
import qualified Data.Vector as V
import Data.Word
import OpenSandbox.Data.Block (BlockStateID)
import OpenSandbox.World.Chunk

classicFlatPreset :: V.Vector BlockStateID
classicFlatPreset = fmap toEnum [112,48,48,32]

genFlatWorld :: ChunkLayers -> Word8 -> Either String [((Int32,Int32), ChunkColumn)]
genFlatWorld layers viewDistance =
  case eitherChunkColumns of
    Left err -> Left err
    Right chunkColumns -> Right $ zip coords chunkColumns
  where
    k = fromIntegral viewDistance
    coords = [(x,z) | x <- [(-k)..k], z <- [(-k)..k]]
    eitherChunkColumns = sequence $ fmap (\(x,z) -> flatChunkColumn x z layers) coords

newtype ChunkLayers = ChunkLayers (V.Vector BlockStateID) deriving (Show,Eq)

mkChunkLayers :: V.Vector BlockStateID -> Either String ChunkLayers
mkChunkLayers layers
  | V.length layers > 256 = Left "Error: Supplied too many layers!"
  | otherwise = Right $ ChunkLayers layers

flatChunkColumn :: Int32 -> Int32 -> ChunkLayers -> Either String ChunkColumn
flatChunkColumn x z layers =
  case flatChunkColumnData layers of
    Left err -> Left err
    Right chunkColumn -> Right $ mkChunkColumn x z chunkColumn flatBiomeIndices V.empty

-- Vector of BlockStateID is ordered bottom up with one BlockStateID per layer
-- of the chunk.
flatChunkColumnData :: ChunkLayers -> Either String ChunkColumnData
flatChunkColumnData (ChunkLayers layers) = do
  let fullLayers = layers V.++ V.replicate (256 - V.length layers) 0
  let neededLn = (ceiling ((fromIntegral . V.length $ layers) / 16)) * 16
  let neededLayers = V.take neededLn fullLayers
  let neededColumn = V.concatMap (V.replicate 256) neededLayers
  let chunked = fmap (\i -> V.slice (i * 4096) 4096 neededColumn) $ take (neededLn `div` 16) $ iterate (+1) 0
  let chunkBlockDats = sequence $ fmap mkChunkBlockData chunked :: Either String [ChunkBlockData]
  let chunkBlocks =
        chunkBlockDats
        >>= (\lst -> sequence $ fmap (\x -> mkChunkBlock x (V.replicate 2048 255) (V.replicate 2048 255)) lst)
  mkChunkColumnData . V.fromList =<< chunkBlocks

flatBiomeIndices :: BiomeIndices
flatBiomeIndices = fillBiomeIndices 1

