-- This file is part of the 'union-find-array' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

-- |
-- Immutable disjoint set forests.
module Data.Union (
    Union,
    Node (..),
    size,
    lookup,
    lookupFlattened,
) where

import Prelude hiding (lookup)
import Data.Union.Type (Union, Node (..))
import qualified Data.Union.Type as T
import Data.Array.Base ((!))

-- | Get the number of nodes in the forest.
size :: Union l -> Int
size = T.size

-- | Look up the representative of a node, and its label.
lookup :: Union l -> Node -> (Node, l)
lookup u (Node n) = go n where
    go n | n' == n   = (Node n, T.label u ! n)
         | otherwise = go n'
      where
        n' = T.up u ! n

-- | Version of 'lookup' that assumes the forest to be flattened.
-- (cf. 'Control.Union.ST.flatten'.)
--
-- Do not use otherwise: It will give wrong results!
lookupFlattened :: Union a -> Node -> (Node, a)
lookupFlattened u (Node n) = (Node (T.up u ! n), T.label u ! n)
