-- |
-- Module      :  ELynx.Tree.Zipper
-- Description :  Zippers on rooted rose trees with branch labels
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jul 23 08:42:37 2020.
module ELynx.Tree.Zipper
  ( -- * Data type
    TreePos (..),

    -- * Conversion
    fromTree,
    toTree,

    -- * Movement
    goParent,
    goParentUnsafe,
    goRoot,
    goLeft,
    goRight,
    goChild,
    goChildUnsafe,

    -- * Paths
    Path,
    goPath,
    goPathUnsafe,
    getSubTreeUnsafe,
    isValidPath,
    isLeafPath,

    -- * Modification
    insertTree,
    modifyTree,
    insertBranch,
    insertLabel,
  )
where

import Data.Foldable
import ELynx.Tree.Rooted

-- | Tree zipper. For reference, please see http://hackage.haskell.org/package/rosezipper.
data TreePos e a = Pos
  { -- | The currently selected tree.
    current :: Tree e a,
    -- | Forest to the left in reversed order.
    before :: Forest e a,
    -- | Forest to the right
    after :: Forest e a,
    -- | Finger to the selected tree
    parents :: [([Tree e a], e, a, [Tree e a])]
  }
  deriving (Show, Eq)

-- | Get a zipper pointing to the root.
fromTree :: Tree e a -> TreePos e a
fromTree t = Pos {current = t, before = [], after = [], parents = []}

-- | Get the complete tree of the zipper.
toTree :: TreePos e a -> Tree e a
toTree = current . goRoot

getForest :: TreePos e a -> Forest e a
getForest pos = foldl (flip (:)) (current pos : after pos) (before pos)

-- | Go to parent.
goParent :: TreePos e a -> Maybe (TreePos e a)
goParent pos = case parents pos of
  (ls, br, lb, rs) : ps ->
    Just
      Pos
        { current = Node br lb $ getForest pos,
          before = ls,
          after = rs,
          parents = ps
        }
  [] -> Nothing

-- | Go to parent.
--
-- Call 'error' if no parent is found.
goParentUnsafe :: TreePos e a -> TreePos e a
goParentUnsafe pos = case parents pos of
  (ls, br, lb, rs) : ps ->
    Pos
      { current = Node br lb $ getForest pos,
        before = ls,
        after = rs,
        parents = ps
      }
  [] -> error "goUpUnsafe: No parent found."

-- | Go to root.
goRoot :: TreePos e a -> TreePos e a
goRoot pos = maybe pos goRoot (goParent pos)

-- | Go to left sibling in current forest.
goLeft :: TreePos e a -> Maybe (TreePos e a)
goLeft pos =
  case before pos of
    t : ts ->
      Just
        pos
          { current = t,
            before = ts,
            after = current pos : after pos
          }
    [] -> Nothing

-- | Go to right sibling in current forest.
goRight :: TreePos e a -> Maybe (TreePos e a)
goRight pos =
  case after pos of
    t : ts ->
      Just
        pos
          { current = t,
            before = current pos : before pos,
            after = ts
          }
    [] -> Nothing

-- | Go to child with given index in forest.
goChild :: Int -> TreePos e a -> Maybe (TreePos e a)
goChild n pos = case current pos of
  (Node br lb ts)
    | null ts -> Nothing
    | length ts <= n -> Nothing
    | otherwise ->
        Just $
          Pos
            { current = head rs',
              before = reverse ls',
              after = tail rs',
              parents = (before pos, br, lb, after pos) : parents pos
            }
    where
      (ls', rs') = splitAt n ts

-- | Go to child with given index in forest. Call 'error' if child does not
-- exist.
goChildUnsafe :: Int -> TreePos e a -> TreePos e a
goChildUnsafe n pos = case current pos of
  (Node br lb ts)
    | null ts -> error "goChildUnsafe: Forest is empty."
    | length ts <= n -> error "goChildUnsafe: Forest is too short."
    | otherwise ->
        Pos
          { current = head rs',
            before = reverse ls',
            after = tail rs',
            parents = (before pos, br, lb, after pos) : parents pos
          }
    where
      (ls', rs') = splitAt n ts

-- | Path from the root of a tree to the node of the tree.
--
-- The position is specific to a tree topology. If the topology changes, the
-- position becomes invalid.
type Path = [Int]

-- | Go to node with given path.
goPath :: Path -> TreePos e a -> Maybe (TreePos e a)
goPath pos pth = foldlM (flip goChild) pth pos

-- | Check if a path is valid in that it leads to a node on a tree.
isValidPath :: Tree e a -> Path -> Bool
isValidPath t p = case goPath p (fromTree t) of
  Nothing -> False
  Just _ -> True

-- | Check if a path leads to a leaf.
isLeafPath :: Tree e a -> Path -> Bool
isLeafPath t p = case goPath p (fromTree t) of
  Nothing -> False
  Just pos -> null $ forest (current pos)

-- | Got to node with given path.
--
-- Call 'error' if path is invalid.
goPathUnsafe :: Path -> TreePos e a -> TreePos e a
goPathUnsafe pos pth =
  {-# SCC "goPathUnsafe" #-}
  foldl (flip goChildUnsafe) pth pos

-- | Get the sub tree at path.
--
-- Call 'error' if path is invalid.
getSubTreeUnsafe :: Path -> Tree e a -> Tree e a
getSubTreeUnsafe p = current . goPathUnsafe p . fromTree

-- | Insert a new tree into the current focus of the zipper.
insertTree :: Tree e a -> TreePos e a -> TreePos e a
insertTree t pos = pos {current = t}

-- | Modify the tree at the current focus of the zipper.
modifyTree :: (Tree e a -> Tree e a) -> TreePos e a -> TreePos e a
modifyTree f pos = pos {current = f t}
  where
    t = current pos

-- | Insert a new branch label into the current focus of the zipper.
insertBranch :: e -> TreePos e a -> TreePos e a
insertBranch br pos = case current pos of
  Node _ lb ts -> pos {current = Node br lb ts}

-- | Insert a new node label into the current focus of the zipper.
insertLabel :: a -> TreePos e a -> TreePos e a
insertLabel lb pos = case current pos of
  Node br _ ts -> pos {current = Node br lb ts}
