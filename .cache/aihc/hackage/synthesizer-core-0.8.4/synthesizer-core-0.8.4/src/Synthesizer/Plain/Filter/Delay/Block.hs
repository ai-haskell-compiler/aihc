{-# LANGUAGE NoImplicitPrelude #-}
{- |
Fast delay based on block lists.
Blocks are arrays. They are part of Haskell 98.
In contrast to ring buffers,
block lists allow infinite look ahead.
-}
module Synthesizer.Plain.Filter.Delay.Block (
   modulated,

   -- for testing
   propDrop,
   ) where

import qualified Synthesizer.Plain.Interpolation as Interpolation
import qualified Synthesizer.Plain.Signal as Sig

import qualified Algebra.RealField as RealField
import qualified Algebra.Additive  as Additive

import Data.Array((!), Array, listArray, elems, bounds, indices, rangeSize)
import Data.List(tails)

import Test.QuickCheck ((==>), Property)

import NumericPrelude.Numeric
import NumericPrelude.Base


modulatedCore :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> Sig.T a -> Sig.T v -> Sig.T v
modulatedCore ip size ts =
   zipWith
      (\t (offset,bs) ->
          let (ti,tf) = splitFraction (-t)
          in  Interpolation.func ip tf (dropBlocksToList (size+offset+ti) bs))
      ts .
   suffixIndexes .
   {- Using 'size' for the block size is a heuristics,
      maybe it is not a good choice in many cases. -}
   listToBlocks size

modulated :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> Sig.T a -> Sig.T v -> Sig.T v
modulated ip maxDelay ts xs =
   let size = maxDelay + Interpolation.number ip
   in  modulatedCore ip
          (size - Interpolation.offset ip)
          ts
          (replicate size zero ++ xs)


type BlockList a = [Array Int a]


listToBlocks :: Int -> Sig.T a -> BlockList a
listToBlocks blockSize =
   map (listArray (0,blockSize-1)) .
   takeWhile (not . null) .
   iterate (drop blockSize)


dropBlocksToList :: Int -> BlockList a -> Sig.T a
dropBlocksToList number blocks =
   let dropUntil remain (b:bs) =
          if remain <= snd (bounds b)
            then (remain, b, bs)
            else dropUntil (remain - rangeSize (bounds b)) bs
       dropUntil remain [] = (remain, listArray (0,-1) [], [])
       (offset, lead, suffix) = dropUntil number blocks
   in  map (lead!) [offset .. (snd $ bounds lead)] ++
       concatMap elems suffix

propDrop :: Int -> Int -> [Int] -> Property
propDrop size n xs =
   let infXs = cycle xs
       len = 1000
   in  size>0 && n>=0 && not (null xs) ==>
          take len (drop n infXs)  ==
          take len (dropBlocksToList n (listToBlocks size infXs))

{- |
Drop elements from a blocked list.
The offset must lie in the leading block.
-}
_dropSingleBlocksToList :: Int -> BlockList a -> Sig.T a
_dropSingleBlocksToList number (arr:arrs) =
   map (arr!) [number .. (snd $ bounds arr)] ++
   concatMap elems arrs
_dropSingleBlocksToList _ [] = []


suffixIndexes :: BlockList a -> [(Int, BlockList a)]
suffixIndexes xs =
   do blockSuffix <- init $ tails xs
      i <- indices $ head blockSuffix
      return (i,blockSuffix)
