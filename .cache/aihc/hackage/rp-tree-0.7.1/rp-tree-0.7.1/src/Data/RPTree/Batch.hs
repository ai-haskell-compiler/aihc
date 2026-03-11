{-# LANGUAGE FlexibleContexts #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Batch (
  treeBatch, forestBatch,
  -- * utils
  dataBatch
  ) where

import Control.Monad (replicateM)
import GHC.Word (Word64)

-- containers
import qualified Data.IntMap.Strict as IM (IntMap, fromList, insert, lookup, map, mapWithKey, traverseWithKey, foldlWithKey, foldrWithKey, intersectionWith)
-- splitmix-distributions
import System.Random.SplitMix.Distributions (Gen, sample, GenT, sampleT, stdNormal)
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM, (!), map, freeze, thaw, take, drop, unzip)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)

import Data.RPTree.Gen (sparse, dense)
import Data.RPTree.Internal (RPTree(..), RPForest, RPT(..), create, createMulti, SVector, Inner(..), Embed(..))

-- | Populate a tree from a dataset
--
-- Assumptions on the data source:
--
-- * non-empty : contains at least one value
treeBatch :: Inner SVector v =>
             Word64 -- ^ random seed
          -> Int -- ^ max tree depth
          -> Int -- ^ min leaf size
          -> Double -- ^ nonzero density of projection vectors
          -> Int -- ^ dimension of projection vectors
          -> V.Vector (Embed v Double x) -- ^ dataset
          -> RPTree Double () (V.Vector (Embed v Double x))
treeBatch seed maxDepth minLeaf pnz dim src =
  let
    rvs = sample seed $ V.replicateM maxDepth (sparse pnz dim stdNormal)
    t = create maxDepth minLeaf rvs src
  in RPTree rvs t

-- | Populate a forest from a data stream
--
-- Assumptions on the data source:
--
-- * non-empty : contains at least one value
forestBatch :: (Inner SVector v) =>
               Word64  -- ^ random seed
            -> Int -- ^ max tree depth, \(l > 1\) 
            -> Int -- ^ min leaf size, \(m_{leaf} > 1\)
            -> Int -- ^ number of trees, \(n_t > 1\)
            -> Double -- ^ nonzero density of projection vectors, \(p_{nz} \in (0, 1)\)
            -> Int -- ^ dimension of projection vectors, \(d > 1\)
            -> V.Vector (Embed v Double x) -- ^ dataset
            -> RPForest Double (V.Vector (Embed v Double x))
forestBatch seed maxd minl ntrees pnz dim src =
  let
    rvss = sample seed $ do
      rvs <- replicateM ntrees $ V.replicateM maxd (sparse pnz dim stdNormal)
      pure $ IM.fromList $ zip [0 .. ] rvs
    ts = createMulti maxd minl rvss src
  in IM.intersectionWith RPTree rvss ts

-- | Batch random data points
dataBatch :: (Monad m, VG.Vector v a) =>
             Int -- ^ number of points to generate
          -> GenT m a -- ^ random point generator
          -> GenT m (v a)
dataBatch n gg = flip VG.unfoldrM 0 $ \i -> do
  if i == n
    then pure Nothing
    else do
    x <- gg
    pure $ Just (x, i + 1)
