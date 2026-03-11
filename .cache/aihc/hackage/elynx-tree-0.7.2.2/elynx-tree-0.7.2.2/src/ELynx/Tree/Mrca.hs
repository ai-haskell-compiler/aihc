-- |
-- Module      :  ELynx.Tree.Mrca
-- Description :  Most recent common ancestors
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Jun 29 15:57:09 2022.
--
-- Specify nodes using most recent common ancestors (MRCA).
module ELynx.Tree.Mrca
  ( isAncestor,
    isMrca,
    getPathToMrca,
    getTreeAtMrca,
    findNode,
  )
where

import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Set as S
import ELynx.Tree.Rooted
import ELynx.Tree.Zipper

-- | Test if the root node of the given tree is an ancestor of the given leaves.
isAncestor :: Ord a => S.Set a -> Tree e a -> Bool
--                      True if an x of xs is not in the collection of leaves.
--                False      if an x of xs is not in the collection of leaves. -> OK.
isAncestor xs t = not $ any (`S.notMember` lvs) xs
  where
    lvs = S.fromList $ leaves t

-- | Test if the root node of the given tree is the MRCA of the given leaves.
isMrca :: Ord a => S.Set a -> Tree e a -> Bool
--                                    True if any daughter forest is an ancestor.
--                               False     if any daughter forest is an ancestor. -> OK.
isMrca xs t = isAncestor xs t && not (any (isAncestor xs) (forest t))

-- | Get the path to the MRCA of the given leaves on the given tree.
--
-- Return 'Left' if:
--
-- - The tree has duplicate leaves.
--
-- - The MRCA cannot be found.
getPathToMrca :: (Ord a, Show a) => S.Set a -> Tree e a -> Either String Path
getPathToMrca ss tr
  | duplicateLeaves tr = Left "getPathToMrca: Tree contains duplicate leaves."
  | otherwise = tail <$> go 0 tr
  where
    go i t
      | isMrca ss t = Right [i]
      --                                    One path will be (Right p).
      | isAncestor ss t =
          case find isRight [go j t' | (j, t') <- zip [0 ..] (forest t)] of
            -- Use 'error' because this should never happen since one subtree has
            -- to contain the MRCA.
            Nothing -> error $ "getPathToMrca: BUG; I am ancestor but no MRCA found for: " <> show ss
            Just xs -> fmap (i :) xs
      | otherwise = Left $ "getPathToMrca: Could not get MRCA for: " <> show ss <> "."

getTreeAtMrca :: (Ord a, Show a) => S.Set a -> Tree e a -> Either String (Tree e a)
getTreeAtMrca ss tr
  | duplicateLeaves tr = Left "getTreeAtMrca: Tree contains duplicate leaves."
  | otherwise = go tr
  where
    go t
      | isMrca ss t = Right t
      --                                One tree will be (Right t).
      | isAncestor ss t = case find isRight $ map go (forest t) of
          -- Use 'error' because this should never happen since one subtree has
          -- to contain the MRCA.
          Nothing -> error $ "getTreeAtMrca: BUG; I am ancestor but no MRCA found for: " <> show ss
          Just x -> x
      | otherwise = Left $ "getTreeAtMrca: Could not get MRCA for: " <> show ss <> "."

-- | Find a node on a tree.
--
-- If the node is found, the node is specified by the MRCA of the returned set
-- of leaves. The set will contain one element if the node is a leaf, or two
-- elements, if the node is internal.
--
-- Return 'Left' if:
--
-- - The tree has duplicate labels.
--
-- - The node cannot be found.
--
-- - The node is a degree two node.
findNode :: (Ord a, Show a) => a -> Tree e a -> Either String (S.Set a)
findNode n t
  | duplicateLabels t = Left "findNode: tree contains duplicate labels"
  | otherwise = case go n t of
      Nothing -> Left $ "findNode: node not found: " <> show n
      Just x -> x
  where
    go x (Node _ y ts)
      | x == y = case ts of
          [] -> Just $ Right $ S.singleton x
          (l : r : _) -> Just $ Right $ S.fromList [head $ leaves l, head $ leaves r]
          _ -> Just $ Left $ "findNode: degree two node found" <> show n
      | otherwise = case mapMaybe (go x) ts of
          [] -> Nothing
          [z] -> Just z
          -- Use 'error' because this should never happen since we check for
          -- duplicate labels.
          _ -> error $ "findNode: BUG; more internal nodes found" <> show n
