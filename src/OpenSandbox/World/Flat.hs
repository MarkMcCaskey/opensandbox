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
  ( ChunkLayers
  , mkChunkLayers
  , flatChunkColumn
  )where

import Data.Int
import qualified Data.Vector as V
import OpenSandbox.Data.Block (BlockStateID)
import OpenSandbox.World.Chunk


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
  let filled = layers V.++ V.replicate (256 - V.length layers) 0
  let dataLoaf = V.concatMap (V.replicate 256) filled
  let slices = fmap (\i -> V.slice (i * 256) (16 * 256) dataLoaf) [0..15]
  let chunkBlockDatas = sequence $ fmap mkChunkBlockData slices :: Either String [ChunkBlockData]
  let chunkBlocks =
        chunkBlockDatas
        >>= (\lst -> sequence $ fmap (\x -> mkChunkBlock x (V.replicate 2048 0) (V.replicate 2048 0)) lst)
  mkChunkColumnData . V.fromList =<< chunkBlocks

flatBiomeIndices :: BiomeIndices
flatBiomeIndices = fillBiomeIndices 0

