module Test.Lazy (tests) where

import qualified Alternative.Lazy as AltLazy

import qualified Data.StorableVector.Lazy as VL
import qualified Data.StorableVector as V
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Word as Word

import Foreign.Storable (Storable)

import Test.QuickCheck (quickCheck)


type W = Word.Word16

compactEqual :: VL.ChunkSize -> [V.Vector W] -> Bool
compactEqual chunkSize chunks =
   let xs = VL.fromChunks chunks
   in  VL.unpack xs == VL.unpack (VL.compact chunkSize xs)

fromChunksLimited ::
   (Storable a) => VL.ChunkSize -> [V.Vector a] -> VL.Vector a
fromChunksLimited (VL.ChunkSize size) =
   VL.fromChunks . List.concatMap (V.sliceVertical size)

compactLimit :: VL.ChunkSize -> [V.Vector W] -> Bool
compactLimit chunkSize =
   all ((<=chunkSize) . VL.chunkSize . V.length) .
   VL.chunks . VL.compact chunkSize . fromChunksLimited chunkSize

compactMax :: VL.ChunkSize -> [V.Vector W] -> Bool
compactMax chunkSize =
   all ((>chunkSize) . VL.chunkSize) .
   ListHT.mapAdjacent (+) . map V.length .
   VL.chunks . VL.compact chunkSize . fromChunksLimited chunkSize

compactAlt :: VL.ChunkSize -> [V.Vector W] -> Bool
compactAlt chunkSize@(VL.ChunkSize size) chunks =
   let xs = fromChunksLimited chunkSize chunks
   in  (map sum $ AltLazy.compact3 size $ map V.length $ VL.chunks xs)
       ==
       (map V.length $ VL.chunks $ VL.compact chunkSize xs)

compactAltLens ::
   (Int -> [Int] -> [[Int]]) -> VL.ChunkSize -> [V.Vector W] -> Bool
compactAltLens compactLens chunkSize@(VL.ChunkSize size) chunks =
   let lens = map V.length $ VL.chunks $ fromChunksLimited chunkSize chunks
   in  AltLazy.compact3 size lens  ==  compactLens size lens


tests :: [(String, IO ())]
tests =
   ("compactEqual", quickCheck compactEqual) :
   ("compactLimit", quickCheck compactLimit) :
   ("compactMax", quickCheck compactMax) :
   ("compactAlt", quickCheck compactAlt) :
   ("compactAlt2", quickCheck $ compactAltLens AltLazy.compact2) :
   ("compactAlt4", quickCheck $ compactAltLens AltLazy.compact4) :
   []
