-- This file is part of the 'union-find-array' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Union.Type (
    Union (..),
    Node (..),
) where

import Data.Array.Unboxed
import Data.Array

-- | An immutable disjoint set forest.
data Union a = Union {
    size  :: !Int,
    up    :: UArray Int Int,
    label :: Array Int a
}

-- | A node in a disjoint set forest.
newtype Node = Node { fromNode :: Int }
    deriving (Eq, Ord, Ix)
