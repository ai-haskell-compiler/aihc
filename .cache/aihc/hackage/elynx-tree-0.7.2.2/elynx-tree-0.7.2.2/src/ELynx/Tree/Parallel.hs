-- |
-- Module      :  ELynx.Tree.Parallel
-- Description :  Evaluation strategies for trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Sep  7 13:36:45 2020.
--
-- Parallel evaluation up to a given layer. By convention layer 0 only has one
-- element: the root node. The layer 1 includes the daughter nodes of the root
-- node, and so on.
--
-- The layer to which a node belongs should not be confused with the 'depth' of
-- a tree.
module ELynx.Tree.Parallel
  ( parTree,
    parBranchFoldMap,
    parLabelFoldMap,
  )
where

import Control.Parallel.Strategies
import Data.Foldable
import ELynx.Tree.Rooted

myParList :: Strategy a -> Strategy [a]
myParList _ [] = return []
myParList s xs = do
  ys <- parList s $ tail xs
  y <- s $ head xs
  return $ y : ys

-- | Parallel evaluation strategy for a tree into normal form.
--
-- Evaluate the sub trees up to given layer in parallel.
parTree :: (NFData e, NFData a) => Int -> Strategy (Tree e a)
parTree n t@(Node br lb ts)
  | n == 1 = do
      ts' <- myParList rdeepseq ts
      return $ Node br lb ts'
  | n >= 2 = do
      ts' <- myParList (parTree (n - 1)) ts
      return $ Node br lb ts'
  | otherwise = rdeepseq t

branchFoldMap :: (e -> f) -> (f -> f -> f) -> Tree e a -> f
branchFoldMap f op (Node br _ ts) = foldl' op (f br) $ map (branchFoldMap f op) ts

-- IDEA: Use and benchmark branch and node specific instances with parFoldMaps.
--
-- @
-- parFoldMap' = blabla
-- parBranchFoldMap' = parFoldMap' . ZipBranchTree
-- parNodeFoldMap' = parFoldMap' . ZipNodeTree
-- @

-- | Map and fold over branches. Evaluate the sub trees up to given layer in parallel.
parBranchFoldMap :: NFData f => Int -> (e -> f) -> (f -> f -> f) -> Tree e a -> f
parBranchFoldMap n f op t@(Node br _ ts)
  | n >= 1 = foldl' op (f br) (map (parBranchFoldMap (n - 1) f op) ts `using` myParList rdeepseq)
  | otherwise = branchFoldMap f op t

nodeFoldMap :: (a -> b) -> (b -> b -> b) -> Tree e a -> b
nodeFoldMap f op (Node _ lb ts) = foldl' op (f lb) $ map (nodeFoldMap f op) ts

-- | Map and fold over labels. Evaluate the sub trees up to given layer in parallel.
parLabelFoldMap :: NFData b => Int -> (a -> b) -> (b -> b -> b) -> Tree e a -> b
parLabelFoldMap n f op t@(Node _ lb ts)
  | n >= 1 = foldl' op (f lb) (map (parLabelFoldMap (n - 1) f op) ts `using` myParList rdeepseq)
  | otherwise = nodeFoldMap f op t
