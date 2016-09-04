{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module       : OpenSandbox.World
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : GPL3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module OpenSandbox.World
  ( BitsPerBlock
  , BitsPerBlockOption (..)
  , mkBitsPerBlock
  , encodeBitsPerBlock
  , decodeBitsPerBlock
  , OverWorldChunkBlock (..)
  , OtherWorldChunkBlock (..)
  , ChunkBlockData (..)
  , initChunkBlockData
  , BiomeIndices (..)
  , encodeIndices
  , decodeIndices
  ) where

import Control.DeepSeq
import Control.Monad
import qualified Data.Attoparsec.ByteString as Decode
import Data.Bits
import qualified Data.ByteString.Builder as Encode
import Data.Foldable
import Data.Int
import qualified Data.List as L
import Data.Maybe
import Data.Serialize
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import GHC.Generics (Generic)
import OpenSandbox.Data.Block (BlockStateID)
import OpenSandbox.Protocol.Serialize
import Debug.Trace

data OverWorldChunkBlock = OverWorldChunkBlock
  { chunkDataArray :: ChunkBlockData
  , chunkBlockLight :: V.Vector Word8
  , chunkSkyLight :: V.Vector Word8
  } deriving (Show,Eq)

initOverWorldChunkBlock :: OverWorldChunkBlock
initOverWorldChunkBlock = OverWorldChunkBlock initChunkBlockData (V.replicate 2048 0) (V.replicate 2048 0)

instance Serialize OverWorldChunkBlock where
  put (OverWorldChunkBlock datArray blockLight skyLight) = do
    put bpb
    putVarInt . V.length $ palette
    when ((V.length palette > 0) && (bpb < 13)) $
      V.mapM_ (putVarInt . fromEnum) palette
    V.mapM_ putWord8 blockLight
    V.mapM_ putWord8 skyLight
    where
      (bpb,palette,indices) = encodeIndices datArray
  get = do
    bpb <- fromIntegral <$> getWord8
    paletteLn <- getVarInt
    palette <- V.replicateM paletteLn (toEnum <$> getVarInt) :: Get (V.Vector BlockStateID)
    datArrayLn <- getVarInt
    datArray <- replicateM datArrayLn getWord64be
    blockLight <- V.replicateM 2048 getWord8
    skyLight <- V.replicateM 2048 getWord8
    let datArray' = decodeIndices (bpb,palette,datArray)
    return $ OverWorldChunkBlock datArray' blockLight skyLight

data OtherWorldChunkBlock = OtherWorldChunkBlock
  { otherDataArray :: ChunkBlockData
  , otherBlockLight :: V.Vector Word8
  } deriving (Show,Eq)

initOtherWorldChunkBlock :: OtherWorldChunkBlock
initOtherWorldChunkBlock = OtherWorldChunkBlock initChunkBlockData (V.replicate 2048 0)

instance Serialize OtherWorldChunkBlock where
  put (OtherWorldChunkBlock datArray blockLight) = do
    put bpb
    putVarInt . V.length $ palette
    when ((V.length palette > 0) && (bpb < 13)) $
      V.mapM_ (putVarInt . fromEnum) palette
    V.mapM_ putWord8 blockLight
    where
      (bpb,palette,indices) = encodeIndices datArray
  get = do
    bpb <- fromIntegral <$> getWord8
    paletteLn <- getVarInt
    palette <- V.replicateM paletteLn (toEnum <$> getVarInt) :: Get (V.Vector BlockStateID)
    datArrayLn <- getVarInt
    datArray <- replicateM datArrayLn getWord64be
    blockLight <- V.replicateM 2048 getWord8
    let datArray' = decodeIndices (bpb,palette,datArray)
    return $ OtherWorldChunkBlock datArray' blockLight

--------------------------------------------------------------------------------

newtype ChunkBlockData = ChunkBlockData (V.Vector BlockStateID) deriving (Show,Eq)

initChunkBlockData :: ChunkBlockData
initChunkBlockData = ChunkBlockData $ V.replicate 4096 0

unChunkBlockData :: ChunkBlockData -> V.Vector BlockStateID
unChunkBlockData (ChunkBlockData blocks) = blocks

instance Serialize ChunkBlockData where
  put (ChunkBlockData blocks) = do
    putVarInt . V.length $ blocks
    V.mapM_ put blocks
  get = do
    count <- getVarInt
    ChunkBlockData <$> V.replicateM count get

--------------------------------------------------------------------------------
{-
newtype PrimaryBitMask = PrimaryBitMask Word16 deriving (Show,Eq,Bits,Generic,NFData)

mkPrimaryBitMask :: ChunkColumnData -> PrimaryBitMask
mkPrimaryBitMask = PrimaryBitMask . V.foldl1' (.|.) . fmap (fromIntegral . chunkY) . unChunkColumnData

unPrimaryBitMask :: PrimaryBitMask -> Word16
unPrimaryBitMask (PrimaryBitMask pbm) = pbm

instance Serialize PrimaryBitMask where
  put (PrimaryBitMask pbm) = putVarInt (fromIntegral pbm)
  get = (PrimaryBitMask . fromIntegral) <$> getVarInt
-}
--------------------------------------------------------------------------------

newtype BiomeIndices = BiomeIndices (V.Vector Word8) deriving (Show,Eq)

initBiomeIndices :: BiomeIndices
initBiomeIndices = BiomeIndices $ V.replicate 256 0

instance Serialize BiomeIndices where
  put (BiomeIndices bi) = V.mapM_ putWord8 bi
  get = BiomeIndices <$> V.replicateM 256 getWord8

--------------------------------------------------------------------------------

type LocalPalette = V.Vector BlockStateID

encodeIndices :: ChunkBlockData -> (BitsPerBlock,LocalPalette,[Word64])
encodeIndices (ChunkBlockData blocks) = (bpb,palette,packIndices bpb 0 0 (V.toList encodedBlocks))
  where
    encodedBlocks = fmap (\x -> fromJust $ V.elemIndex x palette) blocks
    palette = V.fromList . HS.toList . HS.fromList . V.toList $ blocks
    bpb :: BitsPerBlock
    bpb
      | uncheckedBPB < 5 = BitsPerBlock 4
      | (uncheckedBPB > 4) && (uncheckedBPB < 9) = BitsPerBlock (toEnum uncheckedBPB)
      | otherwise = BitsPerBlock 13
    uncheckedBPB :: Int
    uncheckedBPB = (\x -> 64 - x)
      . countLeadingZeros
      . (toEnum :: Int -> Word64)
      . V.length $ palette

decodeIndices :: (BitsPerBlock,LocalPalette,[Word64]) -> ChunkBlockData
decodeIndices (bpb,palette,indices) = ChunkBlockData decodedIndices
  where
    unpackedIndices = V.fromList $ unpackIndices bpb 0 0 indices :: V.Vector Word64
    decodedIndices = V.backpermute palette (fmap fromEnum unpackedIndices)

-- (NOTE) 'a' must also be divisable by BitsPerBlock, otherwise unpacking will pad out the remainder with zeros.
packIndices :: (Bits a, Integral a) => BitsPerBlock -> a -> Int -> [a] -> [Word64]
packIndices (BitsPerBlock bpb) partialL offsetL [] = [(fromIntegral partialL) `shift` (64 - offsetL)]
packIndices (BitsPerBlock bpb) partialL offsetL indices =
  case L.uncons encodeNext of
    Nothing -> [(fromIntegral partialL) `shift` (64 - offsetL) .|. center n encodeFull]
    Just (partialR,encodeLater) -> do
      let encodedLeft = (fromIntegral partialL) `shift` (64 - offsetL)
      let encodedCenter = center n encodeFull
      let encodedRight = (fromIntegral partialR) `shiftR` (bpbI - (64 - (offsetL + n * bpbI)))
      let encodedLong = if offsetR > 0
                           then encodedLeft .|. encodedCenter .|. encodedRight
                           else encodedLeft .|. encodedCenter
      encodedLong : packIndices (BitsPerBlock bpb) partialR (bpbI - offsetR) encodeLater
  where
    bpbI = fromEnum bpb
    (n,offsetR) = (64 - offsetL) `quotRem` bpbI :: (Int,Int)
    (encodeFull,encodeNext) = L.splitAt n indices
    center :: (Bits a, Integral a) => Int -> [a] -> Word64
    center _ [] = 0
    center 1 [x] = fromIntegral x `shift` (64 - offsetL - (n * bpbI))
    center i (x:xs) = fromIntegral x `shift` (64 - offsetL - ((n - i + 1) * bpbI))
                      .|. center (i - 1) xs

unpackIndices :: (Bits a, Integral a) => BitsPerBlock -> Word64 -> Int -> [Word64] -> [a]
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
    center :: (Bits a, Integral a) => Int -> Word64 -> [a]
    center _ 0 = L.replicate n 0
    center 0 _ = []
    center i x = (fromIntegral (x `shiftL` (64 - offsetR - (i * bpbI))) `shiftR` (64 - bpbI)) : center (i - 1) x

newtype BitsPerBlock = BitsPerBlock Word8
  deriving (Show,Eq,Ord,Bits,Num)

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
  toEnum _ = undefined

encodeBitsPerBlock :: BitsPerBlock -> Encode.Builder
encodeBitsPerBlock (BitsPerBlock bpb) = Encode.word8 bpb

decodeBitsPerBlock :: Decode.Parser BitsPerBlock
decodeBitsPerBlock = do
  rawBPB <- Decode.anyWord8
  if (rawBPB >= 4) && (rawBPB <= 13)
    then return $ BitsPerBlock rawBPB
    else fail "Error: Invalid BitsPerBlock value"

--------------------------------------------------------------------------------

--type GlobalPalette = V.Vector BlockStateID

{-
mkGlobalPalette :: [BlockImport] -> GlobalPalette
mkGlobalPalette = V.fromList . L.sort . L.concatMap encodeBlockImport
  where
    getBlockImportId :: BlockImport -> Word16
    getBlockImportId = id
    getMetadata :: Variation -> Word16
    getMetadata = metadata
    encodedBlockID :: BlockImport -> Word16
    encodedBlockID bi = (fromIntegral $ getBlockImportId bi) `shiftL` 4
    encodeBlockImport :: BlockImport -> [BlockStateID]
    encodeBlockImport bi =
      case variations bi of
        Nothing -> [BlockStateID $ toEnum . fromEnum $ encodedBlockID bi]
        Just vlst -> fmap (BlockStateID . toEnum . fromEnum . (\x -> encodedBlockID bi .|. x) . toEnum . fromEnum . getMetadata) vlst
-}

--------------------------------------------------------------------------------

{-
genFlatWorld :: Int32 -> [CBPlay]
genFlatWorld radius = [chunkDataPacket1 x z | x <- [(-radius)..radius], z <- [(-radius)..radius]]
  where
  chunkDataPacket1 x z =
    CBChunkData
    --x
    --z
    --1
    [chunkSection1]
    --(Just $ B.replicate 256 1)
    --V.empty
  chunkSection1 =
    ChunkSection
      (mkBitsPerBlock BitsPerBlock4)
      [0,112,48,32]
      flatWorldBase
      (B.replicate 2048 0)
      (Just (B.replicate 2048 255))
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

-}
