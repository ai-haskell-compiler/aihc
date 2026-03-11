-- This file is part of the 'union-find-array' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

{-# LANGUAGE RankNTypes, FlexibleContexts, CPP #-}
-- |
-- Low-level interface for managing a disjoint set data structure, based on
-- 'Control.Monad.ST'. For a higher level convenience interface, look at
-- 'Control.Monad.Union'.
module Data.Union.ST (
    UnionST,
    runUnionST,
    new,
    grow,
    copy,
    lookup,
    annotate,
    merge,
    flatten,
    size,
    unsafeFreeze,
) where

import qualified Data.Union.Type as U

import Prelude hiding (lookup)
import Control.Monad.ST
import Control.Monad
import Control.Applicative
import Data.Array.Base hiding (unsafeFreeze)
import Data.Array.ST hiding (unsafeFreeze)
import qualified Data.Array.Base as A (unsafeFreeze)

-- | A disjoint set forest, with nodes numbered from 0, which can carry labels.
data UnionST s l = UnionST {
    up :: STUArray s Int Int,
    rank :: STUArray s Int Int,
    label :: STArray s Int l,
    size :: !Int,
    def :: l
}

#if __GLASGOW_HASKELL__ < 702
instance Applicative (ST s) where
    (<*>) = ap
    pure = return
#endif

-- Use http://www.haskell.org/pipermail/libraries/2008-March/009465.html ?

-- | Analogous to 'Data.Array.ST.runSTArray'.
runUnionST :: (forall s. ST s (UnionST s l)) -> U.Union l
runUnionST a = runST $ a >>= unsafeFreeze

-- | Analogous to 'Data.Array.Base.unsafeFreeze'
unsafeFreeze :: UnionST s l -> ST s (U.Union l)
unsafeFreeze u =
    U.Union (size u) <$> A.unsafeFreeze (up u) <*> A.unsafeFreeze (label u)

-- What about thawing?

-- | Create a new disjoint set forest, of given capacity.
new :: Int -> l -> ST s (UnionST s l)
new size def = do
    up <- newListArray (0, size-1) [0..]
    rank <- newArray (0, size-1) 0
    label <- newArray (0, size-1) def
    return UnionST{ up = up, rank = rank, label = label, size = size, def = def }

-- | Grow the capacity of a disjoint set forest. Shrinking is not possible.
-- Trying to shrink a disjoint set forest will return the same forest
-- unmodified.
grow :: UnionST s l -> Int -> ST s (UnionST s l)
grow u size' | size' <= size u = return u
grow u size' = grow' u size'

-- | Copy a disjoint set forest.
copy :: UnionST s l -> ST s (UnionST s l)
copy u = grow' u (size u)

grow' :: UnionST s l -> Int -> ST s (UnionST s l)
grow' u size' = do
    up' <- newListArray (0, size'-1) [0..]
    rank' <- newArray (0, size'-1) 0
    label' <- newArray (0, size'-1) (def u)
    forM_ [0..size u - 1] $ \i -> do
        readArray (up u) i >>= writeArray up' i
        readArray (rank u) i >>= writeArray rank' i
        readArray (label u) i >>= writeArray label' i
    return u{ up = up', rank = rank', label = label', size = size' }

-- | Annotate a node with a new label.
annotate :: UnionST s l -> Int -> l -> ST s ()
annotate u i v = writeArray (label u) i v

-- | Look up the representative of a given node.
--
-- lookup' does path compression.
lookup' :: UnionST s l -> Int -> ST s Int
lookup' u i = do
    i' <- readArray (up u) i
    if i == i' then return i else do
        i'' <- lookup' u i'
        writeArray (up u) i i''
        return i''

-- | Look up the representative of a given node and its label.
lookup :: UnionST s l -> Int -> ST s (Int, l)
lookup u i = do
    i' <- lookup' u i
    l' <- readArray (label u) i'
    return (i', l')

-- | Check whether two nodes are in the same set.
equals :: UnionST s l -> Int -> Int -> ST s Bool
equals u a b = do
    a' <- lookup' u a
    b' <- lookup' u b
    return (a' == b')

-- | Merge two nodes if they are in distinct equivalence classes. The
-- passed function is used to combine labels, if a merge happens.
merge :: UnionST s l -> (l -> l -> (l, a)) -> Int -> Int -> ST s (Maybe a)
merge u f a b = do
    (a', va) <- lookup u a
    (b', vb) <- lookup u b
    if a' == b' then return Nothing else do
        ra <- readArray (rank u) a'
        rb <- readArray (rank u) b'
        let cont x vx y vy = do
                writeArray (label u) y (error "invalid entry")
                let (v, w) = f vx vy
                writeArray (label u) x v
                return (Just w)
        case ra `compare` rb of
            LT -> do
                writeArray (up u) a' b'
                cont b' vb a' va
            GT -> do
                writeArray (up u) b' a'
                cont a' va b' vb
            EQ -> do
                writeArray (up u) a' b'
                writeArray (rank u) b' (ra + 1)
                cont b' vb a' va

-- | Flatten a disjoint set forest, for faster lookups.
flatten :: UnionST s l -> ST s ()
flatten u = forM_ [0..size u - 1] $ lookup' u
