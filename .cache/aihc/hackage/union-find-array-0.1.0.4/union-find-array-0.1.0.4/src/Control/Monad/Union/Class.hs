-- This file is part of the 'union-find-array' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Control.Monad.Union.Class (
    MonadUnion (..),
) where

import Data.Union.Type (Node (..), Union (..))
import Control.Monad.Trans (MonadTrans (..))
import Prelude hiding (lookup)

class Monad m => MonadUnion l m | m -> l where
    -- | Add a new node, with a given label.
    new :: l -> m Node

    -- | Find the node representing a given node, and its label.
    lookup :: Node -> m (Node, l)

    -- | Merge two sets. The first argument is a function that takes the labels
    -- of the corresponding sets' representatives and computes a new label for
    -- the joined set. Returns Nothing if the given nodes are in the same set
    -- already.
    merge :: (l -> l -> (l, a)) -> Node -> Node -> m (Maybe a)

    -- | Re-label a node.
    annotate :: Node -> l -> m ()

    -- | Flatten the disjoint set forest for faster lookups.
    flatten :: m ()

instance (MonadUnion l m, MonadTrans t, Monad (t m)) => MonadUnion l (t m) where
    new a = lift $ new a
    lookup a = lift $ lookup a
    merge a b c = lift $ merge a b c
    annotate a b = lift $ annotate a b
    flatten = lift $ flatten
