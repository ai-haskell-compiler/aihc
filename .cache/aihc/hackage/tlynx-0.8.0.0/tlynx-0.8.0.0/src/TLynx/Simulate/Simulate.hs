{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   Description :  Simulate reconstructed trees
--   Copyright   :  2021 Dominik Schrempf
--   License     :  GPL-3.0-or-later
--
--   Maintainer  :  dominik.schrempf@gmail.com
--   Stability   :  unstable
--   Portability :  portable
--
-- Creation date: Tue Feb 27 17:27:16 2018.
--
-- See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
-- Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005.
--
-- Mon Feb 4 14:26:11 CET 2019: Adding sampling probability rho. See Article
-- (Stadler2009) Stadler, T. On incomplete sampling under birth–death models and
-- connections to the sampling-based coalescent Journal of Theoretical Biology,
-- Elsevier BV, 2009, 261, 58-66
module TLynx.Simulate.Simulate
  ( simulate,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
  ( mapConcurrently,
  )
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (local)
import Control.Parallel.Strategies
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import ELynx.Tools.Reproduction
import ELynx.Tree
import qualified ELynx.Tree.Simulate.Coalescent as CS
import qualified ELynx.Tree.Simulate.PointProcess as PP
import System.Random.Stateful
import TLynx.Grabble
import TLynx.Simulate.Options

-- For a given number of capabilities and number of calculations, get chunk
-- sizes. The chunk sizes will be as evenly distributed as possible and sum up
-- to the number of calculations.
getChunks :: Int -> Int -> [Int]
getChunks c n = ns
  where
    n' = n `div` c
    r = n `mod` c
    ns = replicate r (n' + 1) ++ replicate (c - r) n'

-- | Simulate phylogenetic trees using birth and death process.
simulate :: ELynx SimulateArguments ()
simulate = do
  l@(SimulateArguments nTrees nLeaves pr subS sumS sOpt) <- localArguments <$> ask
  let s = case fromSeedOpt sOpt of
        Nothing -> error "simulate: No seed."
        Just x -> x
  c <- liftIO getNumCapabilities
  logInfoNewSection "Arguments"
  logInfoS $ reportSimulateArguments l
  logInfoNewSection "Simulation"
  logInfoS $ "Number of used cores: " <> show c
  gen <- newIOGenM $ mkStdGen s
  rs <- replicateM c $ splitGenM gen
  gs <- mapM newIOGenM rs
  let chunks = getChunks c nTrees
  trs <- case pr of
    (BirthDeath lambda mu mRho timeSpec) -> do
      let rho = fromMaybe 1.0 mRho
      case subS of
        Nothing -> liftIO $ bdSimulateNTreesConcurrently nLeaves lambda mu rho timeSpec chunks gs
        Just p ->
          bdSimulateAndSubSampleNTreesConcurrently
            nLeaves
            lambda
            mu
            rho
            p
            timeSpec
            chunks
            gs
    Coalescent -> case subS of
      Nothing -> liftIO $ coalSimulateNTreesConcurrently nLeaves chunks gs
      Just p -> coalSimulateAndSubSampleNTreesConcurrently nLeaves p chunks gs
  let ls =
        if sumS
          then parMap rpar (formatNChildSumStat . toNChildSumStat) trs
          else parMap rpar toNewick $ map lengthToPhyloTree trs
  let res = BL.unlines ls
  out "simulated trees" res ".tree"

bdSimulateNTreesConcurrently ::
  (RandomGen g) =>
  Int ->
  Double ->
  Double ->
  Double ->
  PP.TimeSpec ->
  [Int] ->
  [IOGenM g] ->
  IO (Forest Length Int)
bdSimulateNTreesConcurrently nLeaves l m r timeSpec chunks gs = do
  let l' = l * r
      m' = m - l * (1.0 - r)
  trss <-
    mapConcurrently
      (\(n, g) -> PP.simulateNReconstructedTrees n nLeaves timeSpec l' m' g)
      (zip chunks gs)
  return $ concat trss

coalSimulateNTreesConcurrently ::
  (RandomGen g) =>
  Int ->
  [Int] ->
  [IOGenM g] ->
  IO (Forest Length Int)
coalSimulateNTreesConcurrently nL chunks gs = do
  trss <-
    mapConcurrently
      (\(n, g) -> replicateM n $ CS.simulate nL g)
      (zip chunks gs)
  return $ concat trss

bdSimulateAndSubSampleNTreesConcurrently ::
  (RandomGen g) =>
  Int ->
  Double ->
  Double ->
  Double ->
  Double ->
  PP.TimeSpec ->
  [Int] ->
  [IOGenM g] ->
  ELynx SimulateArguments (Forest Length Int)
bdSimulateAndSubSampleNTreesConcurrently nLeaves l m r p timeSpec chunks gs = do
  let nLeavesBigTree = (round $ fromIntegral nLeaves / p) :: Int
      l' = l * r
      m' = m - l * (1.0 - r)
  logInfoNewSection $
    "Simulate one big tree with "
      <> show nLeavesBigTree
      <> " leaves."
  tr <- liftIO $ PP.simulateReconstructedTree nLeavesBigTree timeSpec l' m' (head gs)
  -- Log the base tree.
  logInfoB $ toNewick $ lengthToPhyloTree tr
  logInfoNewSection $
    "Sub sample "
      <> show (sum chunks)
      <> " trees with "
      <> show nLeaves
      <> " leaves."
  let lvs = Seq.fromList $ leaves tr
  trss <-
    liftIO $
      mapConcurrently
        (\(nSamples, g) -> nSubSamples nSamples lvs nLeaves tr g)
        (zip chunks gs)
  let trs = catMaybes $ concat trss
  return $ map prune trs

coalSimulateAndSubSampleNTreesConcurrently ::
  (RandomGen g) =>
  Int ->
  Double ->
  [Int] ->
  [IOGenM g] ->
  ELynx SimulateArguments (Forest Length Int)
coalSimulateAndSubSampleNTreesConcurrently nL p chunks gs = do
  let nLeavesBigTree = (round $ fromIntegral nL / p) :: Int
  logInfoNewSection $
    "Simulate one big tree with "
      <> show nLeavesBigTree
      <> " leaves."
  tr <- liftIO $ CS.simulate nLeavesBigTree (head gs)
  -- Log the base tree.
  logInfoB $ toNewick $ lengthToPhyloTree tr
  logInfoNewSection $
    "Sub sample "
      <> show (sum chunks)
      <> " trees with "
      <> show nL
      <> " leaves."
  let lvs = Seq.fromList $ leaves tr
  trss <-
    liftIO $
      mapConcurrently
        (\(nSamples, g) -> nSubSamples nSamples lvs nL tr g)
        (zip chunks gs)
  let trs = catMaybes $ concat trss
  return $ map prune trs

-- Extract a random subtree with @N@ leaves of a tree with @M@ leaves, where
-- @M>N@ (otherwise error). The complete list of leaves (names are assumed to be
-- unique) has to be provided as a 'Seq.Seq', and a 'Seq.Set', so that fast
-- sub-sampling as well as lookup are fast and so that these data structures do
-- not have to be recomputed when many sub-samples are requested.
nSubSamples ::
  (Ord a, StatefulGen g m) =>
  Int ->
  Seq.Seq a ->
  Int ->
  Tree e a ->
  g ->
  m [Maybe (Tree e a)]
nSubSamples m lvs n tree g
  | Seq.length lvs < n =
      error
        "Given list of leaves is shorter than requested number of leaves."
  | otherwise = do
      lss <- grabble (toList lvs) m n g
      let lsSets = map Set.fromList lss
      return [dropLeavesWith (`Set.notMember` ls) tree | ls <- lsSets]

-- Pair of branch length with number of extant children.
type BrLnNChildren = (Length, Int)

-- Possible summary statistic of phylogenetic trees. A list of tuples
-- (Length, NumberOfExtantChildrenBelowThisBranch).
type NChildSumStat = [BrLnNChildren]

-- Format the summary statistics in the following form:
--
-- @
--   nLeaves1 branchLength1
--   nLeaves2 branchLength2
--   ....
-- @
formatNChildSumStat :: NChildSumStat -> BL.ByteString
formatNChildSumStat s =
  BB.toLazyByteString . mconcat $ map formatNChildSumStatLine s

formatNChildSumStatLine :: BrLnNChildren -> BB.Builder
formatNChildSumStatLine (l, n) =
  BB.intDec n <> BB.char8 ' ' <> BB.doubleDec (fromLength l) <> BB.char8 '\n'

-- Compute NChilSumStat for a phylogenetic tree.
toNChildSumStat :: (HasLength e) => Tree e a -> NChildSumStat
toNChildSumStat (Node br _ []) = [(getLength br, 1)]
toNChildSumStat (Node br _ ts) = (getLength br, sumCh) : concat nChSS
  where
    nChSS = map toNChildSumStat ts
    sumCh = sum $ map (snd . head) nChSS
