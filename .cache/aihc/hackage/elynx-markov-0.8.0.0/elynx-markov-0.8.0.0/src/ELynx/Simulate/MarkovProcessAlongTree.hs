{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   Description :  Work with transition probability matrices on rooted trees
--   Copyright   :  2021 Dominik Schrempf
--   License     :  GPLv3
--
--   Maintainer  :  dominik.schrempf@gmail.com
--   Stability   :  unstable
--   Portability :  non-portable (not tested)
--
-- Calculate transition probability matrices, map rate matrices on trees, populate
-- a tree with states according to a stationary distribution, etc.
--
-- The implementation of the Markov process is more than basic and can be improved
-- in a lot of ways.
module ELynx.Simulate.MarkovProcessAlongTree
  ( -- * Single rate matrix.
    simulate,
    simulateAndFlatten,
    simulateAndFlattenPar,

    -- * Mixture models.
    simulateMixtureModel,
    simulateAndFlattenMixtureModel,
    simulateAndFlattenMixtureModelPar,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Tree
import qualified Data.Vector as V
import ELynx.MarkovProcess.RateMatrix
import ELynx.Simulate.MarkovProcess
import System.Random.MWC.Distributions
import System.Random.Stateful

-- XXX @performace. The horizontal concatenation might be slow. If so,
-- 'concatenateSeqs' or 'concatenateAlignments' can be used, which directly
-- appends vectors.

-- A brain f***. As an example, let @xss@ be a list of alignments (i.e., a list
-- of a list of a list of alleles). This function horizontally concatenates the
-- sites. The number of species needs to be same in each alignment. No checks
-- are performed!
horizontalConcat :: [[[a]]] -> [[a]]
horizontalConcat [xs] = xs
horizontalConcat xss = foldr1 (zipWith (++)) xss

toProbTree :: RateMatrix -> Tree Double -> Tree ProbMatrix
toProbTree q = fmap (probMatrix q)

getRootStates ::
  (StatefulGen g m) =>
  Int ->
  StationaryDistribution ->
  g ->
  m [State]
getRootStates n d g = replicateM n $ categorical d g

-- | Simulate a number of sites for a given substitution model. Only the states
-- at the leafs are retained. The states at internal nodes are removed. This has
-- a lower memory footprint.

-- XXX: Improve performance. Use vectors, not lists. I am actually not sure if
-- this improves performance...
simulateAndFlatten ::
  (StatefulGen g m) =>
  Int ->
  StationaryDistribution ->
  ExchangeabilityMatrix ->
  Tree Double ->
  g ->
  m [[State]]
simulateAndFlatten n d e t g = do
  let q = fromExchangeabilityMatrix e d
      pt = toProbTree q t
  is <- getRootStates n d g
  simulateAndFlatten' is pt g

-- This is the heart of the simulation. Take a tree and a list of root states.
-- Recursively jump down the branches to the leafs. Forget states at internal
-- nodes.
simulateAndFlatten' ::
  (StatefulGen g m) =>
  [State] ->
  Tree ProbMatrix ->
  g ->
  m [[State]]
simulateAndFlatten' is (Node p f) g = do
  is' <- mapM (\i -> jump i p g) is
  if null f
    then return [is']
    else concat <$> sequence [simulateAndFlatten' is' t g | t <- f]

-- | See 'simulateAndFlatten', parallel version.
simulateAndFlattenPar ::
  (RandomGen g) =>
  Int ->
  StationaryDistribution ->
  ExchangeabilityMatrix ->
  Tree Double ->
  IOGenM g ->
  IO [[State]]
simulateAndFlattenPar n d e t g = do
  c <- getNumCapabilities
  rs <- replicateM c $ splitGenM g
  gs <- mapM newIOGenM rs
  let chunks = getChunks c n
      q = fromExchangeabilityMatrix e d
      pt = toProbTree q t
  -- The concurrent map returns a list of [[State]] objects. They have to be
  -- concatenated horizontally.
  sss <-
    mapConcurrently
      ( \(num, gen) -> do
          is <- getRootStates num d gen
          simulateAndFlatten' is pt gen
      )
      (zip chunks gs)
  return $ horizontalConcat sss

-- | Simulate a number of sites for a given substitution model. Keep states at
-- internal nodes. The result is a tree with the list of simulated states as
-- node labels.
simulate ::
  (StatefulGen g m) =>
  Int ->
  StationaryDistribution ->
  ExchangeabilityMatrix ->
  Tree Double ->
  g ->
  m (Tree [State])
simulate n d e t g = do
  let q = fromExchangeabilityMatrix e d
      pt = toProbTree q t
  is <- getRootStates n d g
  simulate' is pt g

-- This is the heart of the simulation. Take a tree and a list of root states.
-- Recursively jump down the branches to the leafs.
simulate' ::
  (StatefulGen g m) =>
  [State] ->
  Tree ProbMatrix ->
  g ->
  m (Tree [State])
simulate' is (Node p f) g = do
  is' <- mapM (\i -> jump i p g) is
  f' <- sequence [simulate' is' t g | t <- f]
  return $ Node is' f'

toProbTreeMixtureModel ::
  V.Vector RateMatrix -> Tree Double -> Tree (V.Vector ProbMatrix)
toProbTreeMixtureModel qs =
  -- XXX: This function is slow. Parallelization?
  fmap (\a -> V.map (`probMatrix` a) qs)

getComponentsAndRootStates ::
  (StatefulGen g m) =>
  Int ->
  V.Vector Double ->
  V.Vector StationaryDistribution ->
  g ->
  m ([Int], [State])
getComponentsAndRootStates n ws ds g = do
  cs <- replicateM n $ categorical ws g
  is <- sequence [categorical (ds V.! c) g | c <- cs]
  return (cs, is)

-- | Simulate a number of sites for a given set of substitution models with
-- corresponding weights. Forget states at internal nodes. See also
-- 'simulateAndFlatten'.
simulateAndFlattenMixtureModel ::
  (StatefulGen g m) =>
  Int ->
  V.Vector Double ->
  V.Vector StationaryDistribution ->
  V.Vector ExchangeabilityMatrix ->
  Tree Double ->
  g ->
  -- | (IndicesOfComponents, [SimulatedSequenceForEachTip])
  m ([Int], [[State]])
simulateAndFlattenMixtureModel n ws ds es t g = do
  let qs = V.zipWith fromExchangeabilityMatrix es ds
      pt = toProbTreeMixtureModel qs t
  (cs, is) <- getComponentsAndRootStates n ws ds g
  ss <- simulateAndFlattenMixtureModel' is cs pt g
  return (cs, ss)

simulateAndFlattenMixtureModel' ::
  (StatefulGen g m) =>
  [State] ->
  [Int] ->
  Tree (V.Vector ProbMatrix) ->
  g ->
  m [[State]]
simulateAndFlattenMixtureModel' is cs (Node ps f) g = do
  is' <- sequence [jump i (ps V.! c) g | (i, c) <- zip is cs]
  if null f
    then return [is']
    else
      concat
        <$> sequence [simulateAndFlattenMixtureModel' is' cs t g | t <- f]

getChunks :: Int -> Int -> [Int]
getChunks c n = ns
  where
    n' = n `div` c
    r = n `mod` c
    ns = replicate r (n' + 1) ++ replicate (c - r) n'

-- NOTE: We could move away from IO here, but moving away from 'mapConcurrently'
-- requires benchmarks. I am just not sure if it makes sense to spend more time
-- on this since the parallelization itself is a bit weird. Like so, we walk
-- along separate trees in each process.
parComp :: (RandomGen g) => Int -> (Int -> IOGenM g -> IO b) -> IOGenM g -> IO [b]
parComp num fun gen = do
  ncap <- getNumCapabilities
  let chunks = getChunks ncap num
  rs <- replicateM ncap $ splitGenM gen
  gs <- mapM newIOGenM rs
  mapConcurrently (uncurry fun) (zip chunks gs)

-- | See 'simulateAndFlattenMixtureModel', parallel version.
simulateAndFlattenMixtureModelPar ::
  (RandomGen g) =>
  Int ->
  V.Vector Double ->
  V.Vector StationaryDistribution ->
  V.Vector ExchangeabilityMatrix ->
  Tree Double ->
  IOGenM g ->
  IO ([Int], [[State]])
simulateAndFlattenMixtureModelPar n ws ds es t g = do
  let qs = V.zipWith fromExchangeabilityMatrix es ds
      pt = toProbTreeMixtureModel qs t
  -- The concurrent computation returns a list of ([Int], [[State]]) objects.
  -- They have to be concatenated horizontally.
  csss <-
    parComp
      n
      ( \n' g' ->
          do
            (cs, is) <- getComponentsAndRootStates n' ws ds g'
            ss <- simulateAndFlattenMixtureModel' is cs pt g'
            return (cs, ss)
      )
      g
  return (concatMap fst csss, horizontalConcat $ map snd csss)

-- | Simulate a number of sites for a given set of substitution models with
-- corresponding weights. Keep states at internal nodes. See also
-- 'simulate'.
simulateMixtureModel ::
  (StatefulGen g m) =>
  Int ->
  V.Vector Double ->
  V.Vector StationaryDistribution ->
  V.Vector ExchangeabilityMatrix ->
  Tree Double ->
  g ->
  m (Tree [State])
simulateMixtureModel n ws ds es t g = do
  let qs = V.zipWith fromExchangeabilityMatrix es ds
      pt = toProbTreeMixtureModel qs t
  (cs, is) <- getComponentsAndRootStates n ws ds g
  simulateMixtureModel' is cs pt g

-- See 'simulateAlongProbTree', only we have a number of mixture components. The
-- starting states and the components for each site have to be provided.
simulateMixtureModel' ::
  (StatefulGen g m) =>
  [State] ->
  [Int] ->
  Tree (V.Vector ProbMatrix) ->
  g ->
  m (Tree [State])
simulateMixtureModel' is cs (Node ps f) g = do
  is' <- sequence [jump i (ps V.! c) g | (i, c) <- zip is cs]
  f' <- sequence [simulateMixtureModel' is' cs t g | t <- f]
  return $ Node is' f'
