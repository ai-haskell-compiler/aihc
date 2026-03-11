{-# language DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language BangPatterns #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-unused-top-binds #-}
{-# options_ghc -Wno-type-defaults #-}
module Data.RPTree.Conduit
  (
    tree,
  forest,
  RPTreeConfig(..),
  rpTreeCfg
  -- ** helpers
  , dataSource
  , liftC
  )
where

import Control.Monad (replicateM)
import Data.Functor (void)
import GHC.Word (Word64)
import GHC.Stack (HasCallStack)

-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await, transPipe)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, last, scanl, print, foldl)
import qualified Data.Conduit.List as C (chunksOf, unfold, unfoldM, mapAccum)
-- containers
import qualified Data.IntMap.Strict as IM (IntMap, fromList, insert, lookup, map, mapWithKey, traverseWithKey, foldlWithKey, foldrWithKey, intersectionWith)
-- splitmix-distributions
import System.Random.SplitMix.Distributions (Gen, sample, GenT, sampleT, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)
import Control.Monad.Trans.Class (MonadTrans(..))
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM, (!), map, freeze, thaw, take, drop, unzip)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)

import Data.RPTree.Gen (sparse, dense)
import Data.RPTree.Internal (RPTree(..), RPForest, RPT(..), insert, insertMulti, levels, points, Inner(..), innerSD, innerSS, metricSSL2, metricSDL2, SVector(..), fromListSv, DVector(..), fromListDv, partitionAtMedian, RPTError(..), Embed(..))
import qualified Data.RPTree.Internal.MedianHeap as MH (MedianHeap, insert, median)

liftC :: (Monad m, MonadTrans t) => C.ConduitT i o m r -> C.ConduitT i o (t m) r
liftC = C.transPipe lift

-- | Populate a tree from a data stream
--
-- Assumptions on the data source:
--
-- * non-empty : contains at least one value
--
-- * stationary : each chunk is representative of the whole dataset
--
-- * bounded : we wait until the end of the stream to produce a result
tree :: (Monad m, Inner SVector v) =>
        Word64 -- ^ random seed
     -> Int -- ^ max tree depth
     -> Int -- ^ min leaf size
     -> Int -- ^ data chunk size
     -> Double -- ^ nonzero density of projection vectors
     -> Int -- ^ dimension of projection vectors
     -> C.ConduitT () (Embed v Double x) m () -- ^ data source
     -> m (RPTree Double () (V.Vector (Embed v Double x)))
tree seed maxDepth minLeaf n pnz dim src = do
  let
    rvs = sample seed $ V.replicateM maxDepth (sparse pnz dim stdNormal)
  t <- C.runConduit $ src .|
                      insertC maxDepth minLeaf n rvs
  pure $ RPTree rvs t





-- | Incrementally build a tree
insertC :: (Monad m, Inner u v, Ord d, VU.Unbox d, Fractional d) =>
           Int -- ^ max tree depth
        -> Int -- ^ min leaf size
        -> Int -- ^ data chunk size
        -> V.Vector (u d) -- ^ random projection vectors
        -> C.ConduitT
           (Embed v d x)
           o
           m
           (RPT d () (V.Vector (Embed v d x))) 
insertC maxDepth minLeaf n rvs = chunkedAccum n z (insert maxDepth minLeaf rvs)
  where
    z = Tip () mempty



-- | Populate a forest from a data stream
--
-- Assumptions on the data source:
--
-- * non-empty : contains at least one value
--
-- * stationary : each chunk is representative of the whole dataset
--
-- * bounded : we wait until the end of the stream to produce a result
forest :: (Monad m, Inner SVector v) =>
          Word64 -- ^ random seed
       -> Int -- ^ max tree depth, \(l > 1\) 
       -> Int -- ^ min leaf size, \(m_{leaf} > 1\)
       -> Int -- ^ number of trees, \(n_t > 1\)
       -> Int -- ^ data chunk size, \(n_{chunk} > 3\)
       -> Double -- ^ nonzero density of projection vectors, \(p_{nz} \in (0, 1)\)
       -> Int -- ^ dimension of projection vectors, \(d > 1\)
       -> C.ConduitT () (Embed v Double x) m () -- ^ data source
       -> m (RPForest Double (V.Vector (Embed v Double x)))
forest seed maxd minl ntrees chunksize pnz dim src = do
  let
    rvss = sample seed $ do
      rvs <- replicateM ntrees $ V.replicateM maxd (sparse pnz dim stdNormal)
      pure $ IM.fromList $ zip [0 .. ] rvs
  ts <- C.runConduit $ src .|
                       insertMultiC maxd minl chunksize rvss
  pure $ IM.intersectionWith RPTree rvss ts

data RPTreeConfig = RPCfg {
  fpMaxTreeDepth :: Int -- ^ max tree depth \(l > 1\) 
  -- , fpMinLeafSize :: Int -- ^ min leaf size 
  , fpDataChunkSize :: Int -- ^ data chunk size
  , fpProjNzDensity :: Double -- ^ nonzero density of projection vectors \(p_{nz} \in (0, 1)\)
                          } deriving (Show)


-- | Configure the rp-tree tree construction process with some natural defaults
rpTreeCfg :: Int -- ^ min leaf size
          -> Int -- ^ number of points in the dataset
          -> Int -- ^ vector dimension of the data points
          -> RPTreeConfig
rpTreeCfg minl n d = RPCfg maxd nchunk pnz
  where
    maxd = ceiling $ logBase 2 (fromIntegral n / fromIntegral minl)
    nchunk = ceiling $ fromIntegral n / 100
    pnzMin = 1 / logBase 10 (fromIntegral d)
    pnz = pnzMin `min` 1.0





insertMultiC :: (Monad m, Ord d, Inner u v, VU.Unbox d, Fractional d, VG.Vector v1 (u d)) =>
                Int  -- ^ max tree depth
             -> Int -- ^ min leaf size
             -> Int -- ^ chunk size
             -> IM.IntMap (v1 (u d)) -- one entry per tree
             -> C.ConduitT
                (Embed v d x)
                o
                m
                (IM.IntMap (RPT d () (V.Vector (Embed v d x))))
insertMultiC maxd minl n rvss = chunkedAccum n im0 (insertMulti maxd minl rvss)
  where
    im0 = IM.map (const z) rvss
    z = Tip () mempty







-- | Aggregate the input stream in chunks of a given size (semantics of 'C.chunksOf'), and fold over the resulting stream building up an accumulator structure (e.g. a tree)
chunkedAccum :: (Monad m) =>
                Int -- ^ chunk size
             -> t -- ^ initial accumulator state
             -> (t -> V.Vector a -> t)
             -> C.ConduitT a o m t
chunkedAccum n z f = C.chunksOf n .|
                     C.map V.fromList .|
                     C.foldl f z

-- | Source of random data points
dataSource :: (Monad m) =>
              Int -- ^ number of vectors to generate
           -> GenT m a -- ^ random generator for the vector components
           -> C.ConduitT i a (GenT m) ()
dataSource n gg = flip C.unfoldM 0 $ \i -> do
  if i == n
    then pure Nothing
    else do
      x <- gg
      pure $ Just (x, i + 1)




-- -- sinks

-- tree' :: (Monad m, Inner SVector v) =>
--         Word64 -- ^ random seed
--      -> Int -- ^ max tree depth
--      -> Int -- ^ min leaf size
--      -> Int -- ^ data chunk size
--      -> Double -- ^ nonzero density of projection vectors
--      -> Int -- ^ dimension of projection vectors
--       -> C.ConduitT (v Double) o m (RPTree Double (V.Vector (v Double)))
-- tree' seed maxDepth minLeaf n pnz dim = do
--   let
--     rvs = sample seed $ V.replicateM maxDepth (sparse pnz dim stdNormal)
--   t <- insertC maxDepth minLeaf n rvs
--   pure $ RPTree rvs t

-- forest' :: (Monad m, Inner SVector v) =>
--            Word64 -- ^ random seed
--         -> Int -- ^ max tree depth
--         -> Int -- ^ min leaf size
--         -> Int -- ^ number of trees
--         -> Int -- ^ data chunk size
--         -> Double -- ^ nonzero density of projection vectors
--         -> Int -- ^ dimension of projection vectors
--         -> C.ConduitT (v Double) o m (IM.IntMap (RPTree Double (V.Vector (v Double))))
-- forest' seed maxd minl ntrees chunksize pnz dim = do
--   let
--     rvss = sample seed $ do
--       rvs <- replicateM ntrees $ V.replicateM maxd (sparse pnz dim stdNormal)
--       pure $ IM.fromList $ zip [0 .. ] rvs
--   ts <- insertMultiC maxd minl chunksize rvss
--   pure $ IM.intersectionWith RPTree rvss ts
