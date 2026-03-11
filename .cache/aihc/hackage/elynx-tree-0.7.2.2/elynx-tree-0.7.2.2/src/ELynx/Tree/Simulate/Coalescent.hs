-- |
-- Module      :  ELynx.Tree.Simulate.Coalescent
-- Description :  Generate coalescent trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed May 16 13:13:11 2018.
module ELynx.Tree.Simulate.Coalescent
  ( simulate,
  )
where

import ELynx.Tree.Distribution.CoalescentContinuous
import ELynx.Tree.Length
import ELynx.Tree.Rooted
import Statistics.Distribution
import System.Random.Stateful

-- | Simulate a coalescent tree with @n@ leaves. The branch lengths are in units
-- of effective population size.
simulate ::
  StatefulGen g m =>
  -- | Number of leaves.
  Int ->
  g ->
  m (Tree Length Int)
simulate n = simulate' n 0 trs
  where
    trs = [Node 0 i [] | i <- [0 .. n - 1]]

simulate' ::
  StatefulGen g m =>
  Int ->
  Int ->
  Forest Length Int ->
  g ->
  m (Tree Length Int)
simulate' n a trs g
  | n <= 0 = error "Cannot construct trees without leaves."
  | n == 1 && length trs /= 1 = error "Too many trees provided."
  | n == 1 && length trs == 1 = return $ head trs
  | otherwise = do
      -- Indices of the leaves to join will be i-1 and i.
      i <- uniformRM (1, n - 1) g
      -- The time of the coalescent event.
      t <- toLengthUnsafe <$> genContVar (coalescentDistributionCont n) g
      let trs' = map (modifyStem (+ t)) trs -- Move time 't' up on the tree.
          tl = trs' !! (i - 1)
          tr = trs' !! i
          -- Join the two chosen trees.
          tm = Node 0 a [tl, tr]
          -- Take the trees on the left, the merged tree, and the trees on the right.
          trs'' = take (i - 1) trs' ++ [tm] ++ drop (i + 1) trs'
      simulate' (n - 1) a trs'' g
