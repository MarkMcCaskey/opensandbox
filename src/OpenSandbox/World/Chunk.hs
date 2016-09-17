{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.World.Chunk
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.World.Chunk
  ( BitsPerBlock
  , BitsPerBlockOption (..)
  , mkBitsPerBlock
  , ChunkColumn
  , mkChunkColumn
  , ChunkColumnData
  , mkChunkColumnData
  , unChunkColumnData
  , ChunkBlock
  , mkChunkBlock
  , ChunkBlockData
  , mkChunkBlockData
  , unChunkBlockData
  , fillChunkBlockData
  , BiomeIndices
  , mkBiomeIndices
  , unBiomeIndices
  , fillBiomeIndices
  , BlockIndices (..)
  , PrimaryBitMask
  , mkPrimaryBitMask
  , unPrimaryBitMask
  , unBlockIndices
  , compressIndices
  , decompressIndices
  , packIndices
  , unpackIndices
  , calcPalette
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import Data.Int
import qualified Data.List as L
import Data.Maybe
import Data.NBT
import Data.Serialize
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)
import OpenSandbox.Data.Block (BlockStateID,BlockIndice)
import OpenSandbox.Protocol.Types (putVarInt,getVarInt)

--------------------------------------------------------------------------------

data ChunkColumn = ChunkColumn
  { _chunkX :: Int32
  , _chunkZ :: Int32
  , _primaryBitMask :: PrimaryBitMask
  , _chunkColumnData :: ChunkColumnData
  , _biomesArray :: BiomeIndices
  , _blockEntities :: V.Vector NBT
  } deriving (Show,Eq)

instance Serialize ChunkColumn where
  put (ChunkColumn cX cZ bitMask chunks biomes entities) = do
    put cX
    put cZ
    put bitMask
    put True
    put chunks
    put biomes
    putVarInt . V.length $ entities
    V.mapM_ put entities

  get = do
    cX <- getInt32be
    cZ <- getInt32be
    bitMask <- get
    _ <- get :: Get Bool
    chunks <- get
    biomes <- get
    ln <- getVarInt
    entities <- V.replicateM ln get
    return $ ChunkColumn cX cZ bitMask chunks biomes entities

mkChunkColumn :: Int32 -> Int32 -> ChunkColumnData -> BiomeIndices -> V.Vector NBT -> ChunkColumn
mkChunkColumn cX cZ chunks biomes entities =
  ChunkColumn cX cZ (mkPrimaryBitMask chunks) chunks biomes entities

--------------------------------------------------------------------------------

data ChunkColumnData = ChunkColumnData (V.Vector ChunkBlock) deriving (Show,Eq)

instance Serialize ChunkColumnData where
  put (ChunkColumnData chunks)= do
    let bs = runPut (V.mapM_ put chunks)
    putVarInt . (+256) . B.length $ bs
    putByteString bs

  get = do
    ln <- ((\x -> x - 256) <$> getVarInt)
    bs <- getBytes ln
    case runGet (many1 get) bs of
      Left err -> fail err
      Right chunks -> return $ ChunkColumnData (V.fromList chunks)
    where
      many1 :: Alternative f => f a -> f [a]
      many1 g = liftA2 (:) g (many g)

mkChunkColumnData :: V.Vector ChunkBlock -> Either String ChunkColumnData
mkChunkColumnData chunks
  | length chunks <= 16 = Right (ChunkColumnData chunks)
  | otherwise = Left "Error: Can only have up to 16 chunks per column!"

unChunkColumnData :: ChunkColumnData -> V.Vector ChunkBlock
unChunkColumnData (ChunkColumnData column) = column

--------------------------------------------------------------------------------

data ChunkBlock = ChunkBlock
  { _chunkBPB :: BitsPerBlock
  , _chunkPalette :: Palette
  , _chunkDataArray :: ChunkBlockData
  , _chunkBlockLight :: V.Vector Word8
  , _chunkSkyLight :: V.Vector Word8
  } deriving (Show,Eq)

mkChunkBlock :: ChunkBlockData -> V.Vector Word8 -> V.Vector Word8 -> Either String ChunkBlock
mkChunkBlock dat blockLight skyLight
  | ((V.length . unChunkBlockData $ dat) == 4096)
    && (V.length blockLight == 2048)
    && (V.length skyLight == 2048) =
      Right $ ChunkBlock bpb palette dat blockLight skyLight
  | otherwise = Left "Error: Invalid length of ChunkBlockData!"
  where
    (bpb,palette) = calcPalette dat

instance Serialize ChunkBlock where
  put (ChunkBlock bpb palette datArray blockLight skyLight) = do
    let indices = compressIndices bpb palette datArray
    put bpb
    putVarInt . V.length $ palette
    V.mapM_ (putVarInt . fromIntegral) palette
    putVarInt . length $ indices
    mapM_ putWord64be indices
    V.mapM_ putWord8 blockLight
    V.mapM_ putWord8 skyLight

  get = do
    bpb <- get
    paletteLn <- getVarInt
    palette <- V.replicateM paletteLn (fromIntegral <$> getVarInt)
    datArrayLn <- getVarInt
    datArray <- replicateM datArrayLn getWord64be
    blockLight <- V.replicateM 2048 getWord8
    skyLight <- V.replicateM 2048 getWord8
    let datArray' = decompressIndices bpb palette datArray
    return $ ChunkBlock bpb palette datArray' blockLight skyLight

--------------------------------------------------------------------------------

newtype ChunkBlockData = ChunkBlockData (V.Vector BlockStateID) deriving (Show,Eq)

mkChunkBlockData :: V.Vector BlockStateID -> Either String ChunkBlockData
mkChunkBlockData blocks = if V.length blocks == 4096
                             then Right $ ChunkBlockData blocks
                             else Left "Error: Invalid number of BlockStateIDs, must be 4096!"

unChunkBlockData :: ChunkBlockData -> V.Vector BlockStateID
unChunkBlockData (ChunkBlockData blocks) = blocks

fillChunkBlockData :: BlockStateID -> ChunkBlockData
fillChunkBlockData = ChunkBlockData . V.replicate 4096

--------------------------------------------------------------------------------

newtype PrimaryBitMask = PrimaryBitMask Word16 deriving (Show,Eq,Bits,Generic,NFData)

instance Serialize PrimaryBitMask where
  put (PrimaryBitMask pbm) = putVarInt (fromIntegral pbm)
  get = (PrimaryBitMask . fromIntegral) <$> getVarInt

mkPrimaryBitMask :: ChunkColumnData -> PrimaryBitMask
mkPrimaryBitMask = PrimaryBitMask . foldr1 (.|.) . fmap (2^) . (\n -> take n $ iterate (+1) (0 :: Word16)) . length . unChunkColumnData

unPrimaryBitMask :: PrimaryBitMask -> Word16
unPrimaryBitMask (PrimaryBitMask pbm) = pbm

--------------------------------------------------------------------------------

newtype BiomeIndices = BiomeIndices (V.Vector Word8) deriving (Show,Eq)

instance Serialize BiomeIndices where
  put (BiomeIndices bi) = V.mapM_ putWord8 bi
  get = BiomeIndices <$> V.replicateM 256 getWord8

mkBiomeIndices :: V.Vector Word8 -> Either String BiomeIndices
mkBiomeIndices indices
  | V.length indices == 256 = Right $ BiomeIndices indices
  | otherwise = Left "Error: Provide exactly 256 biome indices!"

unBiomeIndices :: BiomeIndices -> V.Vector Word8
unBiomeIndices (BiomeIndices indices) = indices

fillBiomeIndices :: Word8 -> BiomeIndices
fillBiomeIndices = BiomeIndices . V.replicate 256

--------------------------------------------------------------------------------

newtype BlockIndices = BlockIndices [BlockIndice] deriving (Show,Eq)

unBlockIndices :: BlockIndices -> [BlockIndice]
unBlockIndices (BlockIndices indices) = indices

type Palette = V.Vector BlockStateID

calcPalette :: ChunkBlockData -> (BitsPerBlock,Palette)
calcPalette (ChunkBlockData blocks) = (bpb,palette)
  where
    palette = V.fromList . HS.toList . HS.fromList . V.toList $ blocks
    bpb :: BitsPerBlock
    bpb
      | uncheckedBPB < 5 = BitsPerBlock 4
      | (uncheckedBPB > 4) && (uncheckedBPB < 9) = BitsPerBlock (toEnum uncheckedBPB)
      | otherwise = BitsPerBlock 16
    uncheckedBPB :: Int
    uncheckedBPB = (\x -> 16 - x)
      . countLeadingZeros
      . (toEnum :: Int -> Word16)
      . V.length $ palette

compressIndices :: BitsPerBlock -> Palette -> ChunkBlockData -> [Word64]
compressIndices bpb palette (ChunkBlockData blocks) = packIndices bpb 0 0 compressedBlocks
  where
    compressedBlocks :: BlockIndices
    compressedBlocks = BlockIndices . V.toList
      $ fmap (\x -> fromIntegral $ fromJust $ V.elemIndex x palette) blocks

decompressIndices :: BitsPerBlock -> Palette -> [Word64] -> ChunkBlockData
decompressIndices bpb palette indices = ChunkBlockData decompressedIndices
  where
    unpackedIndices = V.fromList $ unpackIndices bpb 0 0 indices :: V.Vector BlockIndice
    decompressedIndices = V.backpermute palette (fmap fromEnum unpackedIndices)

packIndices :: BitsPerBlock -> BlockIndice -> Int -> BlockIndices -> [Word64]
packIndices (BitsPerBlock bpb) partialL offsetL (BlockIndices []) =
  [fromIntegral partialL `shift` (64 - offsetL)]
packIndices (BitsPerBlock bpb) partialL offsetL (BlockIndices indices) =
  case L.uncons encodeNext of
    Nothing -> [fromIntegral partialL `shift` (64 - offsetL) .|. center n encodeFull]
    Just (partialR,encodeLater) -> do
      let encodedLeft = fromIntegral partialL `shift` (64 - offsetL)
      let encodedCenter = center n encodeFull
      let encodedRight = fromIntegral partialR `shiftR` (bpbI - (64 - (offsetL + n * bpbI)))
      let encodedLong = if offsetR > 0
                           then encodedLeft .|. encodedCenter .|. encodedRight
                           else encodedLeft .|. encodedCenter
      encodedLong : packIndices (BitsPerBlock bpb) partialR (bpbI - offsetR) (BlockIndices encodeLater)
  where
    bpbI = fromEnum bpb
    (n,offsetR) = (64 - offsetL) `quotRem` bpbI :: (Int,Int)
    (encodeFull,encodeNext) = L.splitAt n indices
    center :: Int -> [BlockIndice] -> Word64
    center _ [] = 0
    center 1 [x] = fromIntegral x `shift` (64 - offsetL - (n * bpbI))
    center i (x:xs) = fromIntegral x `shift` (64 - offsetL - ((n - i + 1) * bpbI))
                      .|. center (i - 1) xs

unpackIndices :: BitsPerBlock -> Word64 -> Int -> [Word64] -> [BlockIndice]
unpackIndices _ _ _ [] = []
unpackIndices (BitsPerBlock bpb) partialL offsetL (x:xs)
  | (offsetL == 0) && (offsetR == 0) = center n x ++ unpackIndices (BitsPerBlock bpb) 0 0 xs
  | offsetL == 0 = center n x ++ unpackIndices (BitsPerBlock bpb) partialR (bpbI - offsetR) xs
  | offsetR == 0 = fromIntegral rejoinedL : center n x ++ unpackIndices (BitsPerBlock bpb) 0 0 xs
  | otherwise = fromIntegral rejoinedL : center n x ++ unpackIndices (BitsPerBlock bpb) partialR (bpbI - offsetR) xs
  where
    bpbI = fromEnum bpb
    (n,offsetR) = (64 - offsetL) `quotRem` bpbI
    partialR = (x `shiftL` (64 - offsetR)) `shiftR` (64 - offsetR)
    rejoinedL = (partialL `shiftL` offsetL) .|. (x `shiftR` (64 - offsetL))
    center :: Int -> Word64 -> [BlockIndice]
    center _ 0 = L.replicate n 0
    center 0 _ = []
    center i x = fromIntegral ((x `shiftL` (64 - offsetR - (i * bpbI))) `shiftR` (64 - bpbI)) : center (i - 1) x

--------------------------------------------------------------------------------

newtype BitsPerBlock = BitsPerBlock Word8
  deriving (Show,Eq,Ord,Bits)

instance Serialize BitsPerBlock where
  put (BitsPerBlock b) = putWord8 b
  get = (mkBitsPerBlock . toEnum . fromEnum) <$> getWord8

mkBitsPerBlock :: BitsPerBlockOption -> BitsPerBlock
mkBitsPerBlock BitsPerBlock4 = BitsPerBlock 4
mkBitsPerBlock BitsPerBlock5 = BitsPerBlock 5
mkBitsPerBlock BitsPerBlock6 = BitsPerBlock 6
mkBitsPerBlock BitsPerBlock7 = BitsPerBlock 7
mkBitsPerBlock BitsPerBlock8 = BitsPerBlock 8
mkBitsPerBlock BitsPerBlock9 = BitsPerBlock 9
mkBitsPerBlock BitsPerBlock10 = BitsPerBlock 10
mkBitsPerBlock BitsPerBlock11 = BitsPerBlock 11
mkBitsPerBlock BitsPerBlock12 = BitsPerBlock 12
mkBitsPerBlock BitsPerBlock13 = BitsPerBlock 13
mkBitsPerBlock BitsPerBlock14 = BitsPerBlock 14
mkBitsPerBlock BitsPerBlock15 = BitsPerBlock 15
mkBitsPerBlock BitsPerBlock16 = BitsPerBlock 16

data BitsPerBlockOption
  = BitsPerBlock4
  | BitsPerBlock5
  | BitsPerBlock6
  | BitsPerBlock7
  | BitsPerBlock8
  | BitsPerBlock9
  | BitsPerBlock10
  | BitsPerBlock11
  | BitsPerBlock12
  | BitsPerBlock13
  | BitsPerBlock14
  | BitsPerBlock15
  | BitsPerBlock16
  deriving (Show,Eq,Ord)

instance Enum BitsPerBlockOption where
  fromEnum BitsPerBlock4 = 4
  fromEnum BitsPerBlock5 = 5
  fromEnum BitsPerBlock6 = 6
  fromEnum BitsPerBlock7 = 7
  fromEnum BitsPerBlock8 = 8
  fromEnum BitsPerBlock9 = 9
  fromEnum BitsPerBlock10 = 10
  fromEnum BitsPerBlock11 = 11
  fromEnum BitsPerBlock12 = 12
  fromEnum BitsPerBlock13 = 13
  fromEnum BitsPerBlock14 = 14
  fromEnum BitsPerBlock15 = 15
  fromEnum BitsPerBlock16 = 16

  toEnum 4 = BitsPerBlock4
  toEnum 5 = BitsPerBlock5
  toEnum 6 = BitsPerBlock6
  toEnum 7 = BitsPerBlock7
  toEnum 8 = BitsPerBlock8
  toEnum 9 = BitsPerBlock9
  toEnum 10 = BitsPerBlock10
  toEnum 11 = BitsPerBlock11
  toEnum 12 = BitsPerBlock12
  toEnum 13 = BitsPerBlock13
  toEnum 14 = BitsPerBlock14
  toEnum 15 = BitsPerBlock15
  toEnum 16 = BitsPerBlock16
  toEnum _ = undefined
