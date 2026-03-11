{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
-- {-# language MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-unused-top-binds #-}

{-|
Random projection trees for approximate nearest neighbor search in high-dimensional vector spaces.

== Introduction

Similarity search is a common problem in many fields (imaging, natural language processing, ..), and is often one building block of a larger data processing system.

There are many ways to /embed/ data in a vector space such that similarity search can be recast as a geometrical nearest neighbor lookup.

In turn, the efficiency and effectiveness of querying such a vector database strongly depends on how internally the data index is represented, graphs and trees being two common approaches.

The naive, all-pairs exact search becomes impractical even at moderate data sizes, which motivated research into approximate indexing methods.


== Overview

This library provides a /tree/-based approach to approximate nearest neighbor search. The database is recursively partitioned according to a series of random projections, and this partitioning is logically arranged as a tree which allows for rapid lookup.

Internally, a single random projection vector is sampled per tree level, as proposed in [1]. The projection vectors in turn can be sparse with a tunable sparsity parameter, which can help compressing the database at a small accuracy cost.

Retrieval accuracy can be improved by populating multiple trees (i.e. a /random forest/), and intersecting the results of the same query against each of them.

== Quick Start

1) Build an index with 'forest'

2) Lookup the \(k\) nearest neighbors to a query point with 'knn'

3) The database can be serialised and restored with 'serialiseRPForest' and 'deserialiseRPForest', respectively.



== References

1) Hyvonen, V., et al., Fast Nearest Neighbor Search through Sparse Random Projections and Voting,  https://www.cs.helsinki.fi/u/ttonteri/pub/bigdata2016.pdf

-}
module Data.RPTree (
  -- * Construction
  -- ** Batch
  treeBatch
  , forestBatch
  -- ** Incremental (Conduit-based)
  , tree
  , forest
  -- ** Parameters
  , rpTreeCfg, RPTreeConfig(..)
  -- , ForestParams
  -- * k-nearest neighbors queries
  , knn
  , knnH
  , knnPQ
  -- * I/O
  , serialiseRPForest
  , deserialiseRPForest
  -- * Statistics
  , recallWith
  -- * Access
  , leaves, levels, points, candidates
  -- * Validation
  , treeStats, treeSize, leafSizes
  , RPTreeStats
  -- * Types
  , Embed(..)
  -- ** RPTree
  , RPTree, RPForest
  -- * Vector types
  -- ** Sparse
  , SVector, fromListSv, fromVectorSv
  -- ** Dense
  , DVector, fromListDv, fromVectorDv
  -- * Vector space typeclasses
  , Inner(..), Scale(..)
    -- ** Helpers for implementing 'Inner' instances
    -- *** Inner product
  , innerSS, innerSD, innerDD
    -- *** L2 distance
  , metricSSL2, metricSDL2
  -- *** Scale
  , scaleS, scaleD

  -- * Rendering
  -- , draw
  -- ** CSV
  , writeCsv, knnWriteCsv
  -- ** GraphViz dot
  , writeDot
  -- * Testing
  , BenchConfig(..), normalSparse2
  , liftC
  -- ** Random generation
  , randSeed
  -- *** Batch
  , dataBatch
  -- *** Conduit
  , dataSource
  , datS, datD
  -- *** Vector data
  , sparse, dense
  , normal2, circle2d
  ) where

import Control.Monad (replicateM)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (Foldable(..), maximumBy, minimumBy)
import Data.Functor.Identity (Identity(..))
import Data.List (partition, sortBy)
import Data.Maybe (maybeToList)
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import Data.Semigroup (Min(..))
import GHC.Generics (Generic)
import GHC.Word (Word64)

-- containers
import Data.Sequence (Seq, (|>))
import qualified Data.Map as M (Map, fromList, toList, foldrWithKey, insert, insertWith, intersection)
import qualified Data.Set as S (Set, fromList, intersection, insert)
-- deepseq
import Control.DeepSeq (NFData(..))
-- heaps
import qualified Data.Heap as H (Heap, fromList, insert, Entry(..), empty, group, viewMin, map, union)
-- -- psqueues
-- import qualified Data.IntPSQ as PQ (IntPSQ, findMin, minView, empty, insert, fromList, toList)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState, get, put)
import Control.Monad.Trans.Class (MonadTrans(..))
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM, (!), map, freeze, thaw, take, drop, unzip, foldl, fromList)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sortBy)

import Data.RPTree.Batch (treeBatch, forestBatch, dataBatch)
import Data.RPTree.Conduit (tree, forest, dataSource, liftC, rpTreeCfg, RPTreeConfig(..))
import Data.RPTree.Gen (sparse, dense, normal2, normalSparse2, circle2d)
import Data.RPTree.Internal (RPTree(..), RPForest, RPT(..), Embed(..), leaves, levels, points, Inner(..), Scale(..), scaleS, scaleD, (/.), innerDD, innerSD, innerSS, metricSSL2, metricSDL2, SVector(..), fromListSv, fromVectorSv, DVector(..), fromListDv, fromVectorDv, partitionAtMedian, Margin, getMargin, sortByVG, serialiseRPForest, deserialiseRPForest)
import Data.RPTree.Internal.Testing (BenchConfig(..), randSeed, datS, datD)
import Data.RPTree.Draw (writeDot, writeCsv, knnWriteCsv)




-- | Look up the \(k\) nearest neighbors to a query point
--
-- The supplied distance function @d@ must satisfy the definition of a metric, i.e.
--
-- * identity of indiscernible elements : \( d(x, y) = 0 \leftrightarrow x \equiv y \)
--
-- * symmetry : \(  d(x, y) = d(y, x)  \)
--
-- * triangle inequality : \( d(x, y) + d(y, z) \geq d(x, z) \)
knn :: (Ord p, Inner SVector v, VU.Unbox d, Real d) =>
       (u d -> v d -> p) -- ^ distance function
    -> Int -- ^ k neighbors
    -> RPForest d (V.Vector (Embed u d x)) -- ^ random projection forest
    -> v d -- ^ query point
    -> V.Vector (p, Embed u d x) -- ^ ordered in increasing distance order to the query point
knn distf k tts q = VG.take k $ sortByVG fst cs
  where
    cs = VG.map (\xe -> (eEmbed xe `distf` q, xe)) $ fold $ (`candidates` q) <$> tts

-- | Same as 'knn' but based on 'H.Heap'
--
-- FIXME uses 'nub' internally so might be slow
knnPQ :: (Ord p, Inner SVector v, VU.Unbox d, Real d) =>
         (u d -> v d -> p) -- ^ distance function
      -> Int -- ^ k neighbors
      -> RPForest d (V.Vector (Embed u d x)) -- ^ random projection forest
      -> v d -- ^ query point
      -> V.Vector (p, Embed u d x) -- ^ ordered in increasing distance order to the query point
knnPQ distf k tts q = VG.fromList $ take k $ map (\(H.Entry p x) -> (p , x)) $ nub h
  where
    xs = fold $ (`candidates` q) <$> tts
    h = VG.foldl insf H.empty xs
      where
        insf acc x = H.insert (H.Entry p x) acc
          where
            p = eEmbed x `distf` q

-- | Same as 'knn' but based on min-'H.Heap'
--
-- Leaves are prioritized according to their margin
knnH :: (Ord p, Inner SVector v, VU.Unbox d, Fractional d, Ord d) =>
         (u d -> v d -> p) -- ^ distance function
      -> Int -- ^ k neighbors
      -> RPForest d (V.Vector (Embed u d x)) -- ^ random projection forest
      -> v d -- ^ query point
      -> V.Vector (p, Embed u d x) -- ^ ordered in increasing distance order to the query point
knnH distf k tts q = VG.map (\xe -> (eEmbed xe `distf` q, xe)) $ go mempty 0 htot
  where
    htot = unions $ (`candidatesH` q) <$> tts
    go acc n hh = case H.viewMin hh of
          Nothing -> acc
          Just ((H.Entry _ xsh), hrest) ->
            let
              nels = length xsh
              ntot = nels + n
            in
              if ntot > k && not (null acc)
              then acc
              else go (xsh <> acc) ntot hrest


unions :: Foldable t => t (H.Heap a) -> H.Heap a
unions = foldl H.union H.empty

-- | deduplicate
nub :: (Ord a) => H.Heap a -> [a]
nub = foldMap maybeToList . H.map view . H.group
  where
    view h = fst <$> H.viewMin h

type PQueue p a = H.Heap (H.Entry p a)

-- -- | Same as 'knn' but accumulating the result in low margin order (following the intuition in 'annoy').
-- --
-- -- FIXME to be verified
-- knnPQ :: (Ord p, Inner SVector v, VU.Unbox d, RealFrac d) =>
--          (u d -> v d -> p) -- ^ distance function
--       -> Int -- ^ k neighbors
--       -> RPForest d (V.Vector (Embed u d x)) -- ^ random projection forest
--       -> v d -- ^ query point
--       -> V.Vector (p, Embed u d x)
-- knnPQ distf k tts q = sortByVG fst cs
--   where
--     cs = VG.map (\xe -> (eEmbed xe `distf` q, xe)) $ fold cstt
--     cstt = (takeFromPQ nsing) . (`candidatesPQ` q) <$> tts
--     nsing = (k `div` n) `max` 1
--     n = length tts




-- | Average recall-at-k, computed over a set of trees
-- 
-- The supplied distance function @d@ must satisfy the definition of a metric, i.e.
--
-- * identity of indiscernible elements : \( d(x, y) = 0 \leftrightarrow x \equiv y \)
--
-- * symmetry : \(  d(x, y) = d(y, x)  \)
--
-- * triangle inequality : \( d(x, y) + d(y, z) \geq d(x, z) \)
recallWith :: (Inner SVector v, VU.Unbox d, Fractional b, Ord d, Ord a, Ord x, Ord (u d), Num d) =>
              (u d -> v d -> a) -- ^ distance function
           -> RPForest d (V.Vector (Embed u d x))
           -> Int -- ^ k : number of nearest neighbors to consider
           -> v d -- ^ query point
           -> b
recallWith distf tt k q = sum rs / fromIntegral n
  where
    rs = fmap (\t -> recallWith1 distf t k q) tt
    n = length tt

recallWith1 :: (Inner SVector v, Ord d, VU.Unbox d, Fractional p, Ord a, Ord x, Ord (u d), Num d) =>
              (u d -> v d -> a) -- ^ distance function
           -> RPTree d l (V.Vector (Embed u d x))
           -> Int -- ^ k : number of nearest neighbors to consider
           -> v d -- ^ query point
           -> p
recallWith1 distf tt k q = fromIntegral (length aintk) / fromIntegral k
  where
    aintk = aa `S.intersection` kk
    aa = set $ candidates tt q
    kk = S.fromList $ map fst $ take k dists -- first k nn's
    dists = sortBy (comparing snd) $ toList $ fmap (\x -> (x, eEmbed x `distf` q)) xs
    xs = points tt

set :: (Foldable t, Ord a) => t a -> S.Set a
set = foldl (flip S.insert) mempty



{-# SCC candidates #-}
-- | Retrieve points nearest to the query
--
-- in case of a narrow margin, collect both branches of the tree
candidates :: (Inner SVector v, VU.Unbox d, Ord d, Num d, Semigroup xs) =>
              RPTree d l xs
           -> v d -- ^ query point
           -> xs
candidates (RPTree rvs tt) x = go 0 tt
  where
    go _     (Tip _ xs)                     = xs
    go ixLev (Bin _ thr margin ltree rtree) =
      let
        (mglo, mghi) = getMargin margin
        r = rvs VG.! ixLev
        proj = r `inner` x
        i' = succ ixLev
        dl = abs (mglo - proj) -- left margin
        dr = abs (mghi - proj) -- right margin
      in
        if | proj < thr &&
             dl > dr -> go i' ltree <> go i' rtree
           | proj < thr  -> go i' ltree
           | proj > thr &&
             dl < dr -> go i' ltree <> go i' rtree
           | otherwise   -> go i' rtree


-- | RPTree margins used as search priority (using a min-heaps : lower margin leaves will be ranked highest and therefore searched first)
candidatesH :: (Inner SVector v, VU.Unbox d, Ord d, Fractional d) =>
               RPTree d l a
            -> v d -- ^ query point
            -> H.Heap (H.Entry d a)
candidatesH (RPTree rvs tt) x = go 0 tt H.empty infty
  where
    infty = 1 / 0
    go _ (Tip _ xs) acc p = insertp p xs acc
    go ixLev (Bin _ thr margin ltree rtree) acc p =
      let
        (mglo, mghi) = getMargin margin
        r = rvs VG.! ixLev
        proj = r `inner` x
        i' = succ ixLev
        dl = abs (mglo - proj) -- left margin
        dr = abs (mghi - proj) -- right margin
        pl = p `min` dl
        pr = p `min` dr
      in if
        | proj < thr &&
          dl > dr -> H.union (go i' ltree acc pl) (go i' rtree acc pr)
        | proj < thr  -> go i' ltree acc pl
        | proj > thr &&
          dl < dr -> H.union (go i' ltree acc pl) (go i' rtree acc pr)
        | otherwise -> go i' rtree acc pr

insertp :: Ord p =>
           p -> a -> H.Heap (H.Entry p a) -> H.Heap (H.Entry p a)
insertp p x = H.insert (H.Entry p x)




data RPTreeStats = RPTreeStats {
  rptsLength :: Int
                               } deriving (Eq, Show)

treeStats :: RPTree d l a -> RPTreeStats
treeStats (RPTree _ tt) = RPTreeStats l
  where
    l = length tt


-- | How many data items are stored in the 'RPTree'
treeSize :: (Foldable t) => RPTree d l (t a) -> Int
treeSize = sum . leafSizes

-- | How many data items are stored in each leaf of the 'RPTree'
leafSizes :: Foldable t => RPTree d l (t a) -> RPT d l Int
leafSizes (RPTree _ tt) = length <$> tt



-- candidatesPQ :: (Ord a, Ord d, Inner SVector v, VU.Unbox d, Num d) =>
--                 RPTree d l a -> v d -> H.Heap a
-- candidatesPQ (RPTree rvs tt) x = go 0 tt H.empty
--   where
--     go _ (Tip _ xs) acc = H.insert xs acc
--     go ixLev (Bin _ thr margin ltree rtree) acc =
--       let
--         (mglo, mghi) = getMargin margin
--         r = rvs VG.! ixLev
--         proj = r `inner` x
--         i' = succ ixLev
--         dl = abs (mglo - proj) -- left margin
--         dr = abs (mghi - proj) -- right margin
--       in if
--         | proj < thr &&
--           dl > dr -> H.union (go i' ltree acc) (go i' rtree acc)
--         | proj < thr  -> go i' ltree acc
--         | proj > thr &&
--           dl < dr -> H.union (go i' ltree acc) (go i' rtree acc)
--         | otherwise -> go i' rtree acc


-- -- | like 'candidates' but outputs an ordered 'IntPQ' where the margin to the median projection is interpreted as queue priority
-- candidatesPQ :: (Fractional d, Ord d, Inner SVector v, VU.Unbox d) =>
--                RPTree d l xs
--             -> v d -- ^ query point
--             -> PQ.IntPSQ d xs
-- candidatesPQ (RPTree rvs tt) x = evalS $ go 0 tt PQ.empty (1/0)
--   where
--     go _ (Tip _ xs) acc dprev =
--       insPQ dprev xs acc
--     go ixLev (Bin _ thr margin ltree rtree) acc dprev = do
--       let
--         (mglo, mghi) = getMargin margin
--         r = rvs VG.! ixLev
--         proj = r `inner` x
--         i' = succ ixLev
--         dl = abs (mglo - proj) -- left margin
--         dr = abs (mghi - proj) -- right margin
--       if | proj < thr &&
--            dl > dr -> do
--              ll <- go i' ltree acc (min dprev dl)
--              lr <- go i' rtree acc (min dprev dr)
--              pure $ PQ.fromList (PQ.toList ll <> PQ.toList lr)
--          | proj < thr  -> go i' ltree acc (min dprev dl)
--          | proj > thr &&
--            dl < dr -> do
--              ll <- go i' ltree acc (min dprev dl)
--              lr <- go i' rtree acc (min dprev dr)
--              pure $ PQ.fromList (PQ.toList ll <> PQ.toList lr)
--          | otherwise -> go i' rtree acc (min dprev dr)

-- takeFromPQ :: (Ord p, Foldable t, Monoid (t a)) =>
--               Int -- ^ number of elements to keep
--            -> PQ.IntPSQ p (t a)
--            -> t a
-- takeFromPQ n pq = foldMap snd $ reverse $ go [] 0 pq
--   where
--     go acc nacc q = case PQ.minView q of
--       Nothing -> acc
--       Just (_, p, xs, pqRest) ->
--         let
--           nxs = length xs
--           nacc' = nacc + nxs
--         in if nacc' < n
--            then go ((p, xs) : acc) nacc' pqRest
--            else acc

-- type S = State Int
-- evalS :: S a -> a
-- evalS = flip evalState 0

-- insPQ :: (Ord p) => p -> v -> PQ.IntPSQ p v -> S (PQ.IntPSQ p v)
-- insPQ p x pq = do
--   i <- get
--   let
--     pq' = PQ.insert i p x pq
--   put (succ i)
--   pure pq'






-- pqSeq :: Ord a => PQ.IntPSQ a b -> Seq (a, b)
-- pqSeq pqq = go pqq mempty
--   where
--     go pq acc = case PQ.minView pq of
--       Nothing -> acc
--       Just (_, p, v, rest) -> go rest $ acc |> (p, v)


-- newtype Counts a = Counts {
--   unCounts :: M.Map a (Sum Int) } deriving (Eq, Show, Semigroup, Monoid)
-- keepCounts :: Int -- ^ keep entry iff counts are larger than this value
--            -> Counts a
--            -> [(a, Int)]
-- keepCounts thr cs = M.foldrWithKey insf mempty c
--   where
--     insf k v acc
--       | v >= thr = (k, v) : acc
--       | otherwise = acc
--     c = getSum `fmap` unCounts cs
-- counts :: (Foldable t, Ord a) => t a -> Counts a
-- counts = foldl count mempty
-- count :: Ord a => Counts a -> a -> Counts a
-- count (Counts mm) x = Counts $ M.insertWith mappend x (Sum 1) mm


-- forest :: Inner SVector v =>
--           Int -- ^ # of trees
--        -> Int -- ^ maximum tree height
--        -> Double -- ^ nonzero density of sparse projection vectors
--        -> Int -- ^ dimension of projection vectors
--        -> V.Vector (v Double) -- ^ dataset
--        -> Gen [RPTree Double (V.Vector (v Double))]
-- forest nt maxDepth pnz dim xss =
--   replicateM nt (tree maxDepth pnz dim xss)

-- -- | Build a random projection tree
-- --
-- -- Optimization: instead of sampling one projection vector per branch, we sample one per tree level (as suggested in https://www.cs.helsinki.fi/u/ttonteri/pub/bigdata2016.pdf )
-- tree :: (Inner SVector v) =>
--          Int -- ^ maximum tree height
--       -> Double -- ^ nonzero density of sparse projection vectors
--       -> Int -- ^ dimension of projection vectors
--       -> V.Vector (v Double) -- ^ dataset
--       -> Gen (RPTree Double (V.Vector (v Double)))
-- tree maxDepth pnz dim xss = do
--   -- sample all projection vectors
--   rvs <- V.replicateM maxDepth (sparse pnz dim stdNormal)
--   let
--     loop ixLev xs = do
--       if ixLev >= maxDepth || length xs <= 100
--         then
--           pure $ Tip xs
--         else
--         do
--           let
--             r = rvs VG.! ixLev
--             (thr, margin, ll, rr) = partitionAtMedian r xs
--           treel <- loop (ixLev + 1) ll
--           treer <- loop (ixLev + 1) rr
--           pure $ Bin thr margin treel treer
--   rpt <- loop 0 xss
--   pure $ RPTree rvs rpt





-- -- | Partition at median inner product
-- treeRT :: (Monad m, Inner SVector v) =>
--            Int
--         -> Int
--         -> Double
--         -> Int
--         -> V.Vector (v Double)
--         -> GenT m (RT SVector Double (V.Vector (v Double)))
-- treeRT maxDepth minLeaf pnz dim xss = loop 0 xss
--   where
--     loop ixLev xs = do
--       if ixLev >= maxDepth || length xs <= minLeaf
--         then
--           pure $ RTip xs
--         else
--         do
--           r <- sparse pnz dim stdNormal
--           let
--             (_, mrg, ll, rr) = partitionAtMedian r xs
--           treel <- loop (ixLev + 1) ll
--           treer <- loop (ixLev + 1) rr
--           pure $ RBin r mrg treel treer







-- -- | Like 'tree' but here we partition at the median of the inner product values instead
-- tree' :: (Inner SVector v) =>
--          Int
--       -> Double
--       -> Int
--       -> V.Vector (v Double)
--       -> Gen (RPTree Double (V.Vector (v Double)))
-- tree' maxDepth pnz dim xss = do
--   -- sample all projection vectors
--   rvs <- V.replicateM maxDepth (sparse pnz dim stdNormal)
--   let
--     loop ixLev xs =
--       if ixLev >= maxDepth || length xs <= 100
--         then Tip xs
--         else
--           let
--             r = rvs VG.! ixLev
--             (thr, margin, ll, rr) = partitionAtMedian r xs
--             tl = loop (ixLev + 1) ll
--             tr = loop (ixLev + 1) rr
--           in Bin thr margin tl tr
--   let rpt = loop 0 xss
--   pure $ RPTree rvs rpt


-- -- | Partition uniformly at random between inner product extreme values
-- treeRT :: (Monad m, Inner SVector v) =>
--           Int -- ^ max tree depth
--        -> Int -- ^ min leaf size
--        -> Double -- ^ nonzero density
--        -> Int -- ^ embedding dimension
--        -> V.Vector (v Double) -- ^ data
--        -> GenT m (RT SVector Double (V.Vector (v Double)))
-- treeRT maxDepth minLeaf pnz dim xss = loop 0 xss
--   where
--     loop ixLev xs = do
--       if ixLev >= maxDepth || length xs <= minLeaf
--         then
--           pure $ RTip xs
--         else
--         do
--           -- sample projection vector
--           r <- sparse pnz dim stdNormal
--           let
--             -- project the dataset
--             projs = map (\x -> (x, r `inner` x)) xs
--             hi = snd $ maximumBy (comparing snd) projs
--             lo = snd $ minimumBy (comparing snd) projs
--           -- sample a threshold
--           thr <- uniformR lo hi
--           let
--             (ll, rr) = partition (\xp -> snd xp < thr) projs
--           treel <- loop (ixLev + 1) (map fst ll)
--           treer <- loop (ixLev + 1) (map fst rr)
--           pure $ RBin r treel treer


-- -- | Partition wrt a plane _|_ to the segment connecting two points sampled at random
-- --
-- -- (like annoy@@)
-- treeRT2 :: (Monad m, Ord d, Fractional d, Inner v v, VU.Unbox d, Num d) =>
--            Int
--         -> Int
--         -> [v d]
--         -> GenT m (RT v d [v d])
-- treeRT2 maxd minl xss = loop 0 xss
--   where
--     loop ixLev xs = do
--       if ixLev >= maxd || length xs <= minl
--         then
--           pure $ RTip xs
--         else
--         do
--           x12 <- sampleWOR 2 xs
--           let
--             (x1:x2:_) = x12
--             r = x1 ^-^ x2
--             (ll, rr) = partition (\x -> (r `inner` (x ^-^ x1) < 0)) xs
--           treel <- loop (ixLev + 1) ll
--           treer <- loop (ixLev + 1) rr
--           pure $ RBin r treel treer










-- ulid :: MonadIO m => a -> m (ULID a)
-- ulid x = ULID <$> pure x <*> liftIO UU.getULID
-- data ULID a = ULID { uData :: a , uULID :: UU.ULID } deriving (Eq, Show)
-- instance (Eq a) => Ord (ULID a) where
--   ULID _ u1 <= ULID _ u2 = u1 <= u2
