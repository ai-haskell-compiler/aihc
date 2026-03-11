{-|
Module      : Data.Conduit.Algorithms
Copyright   : 2013-2021 Luis Pedro Coelho
License     : MIT
Maintainer  : luis@luispedro.org

Simple algorithms packaged as Conduits
-}
{-# LANGUAGE Rank2Types #-}
module Data.Conduit.Algorithms
    ( uniqueOnC
    , uniqueC
    , removeRepeatsC
    , mergeC
    , mergeC2
    ) where

import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Set as S
import qualified Data.PriorityQueue.FingerTree as PQ
import           Control.Monad.Trans.Class (lift)
import           Control.Monad (foldM)

import           Data.Conduit.Algorithms.Utils (awaitJust)


-- | Unique conduit.
--
-- For each element, it checks its key (using the @a -> b@ key function) and
-- yields it if it has not seen it before.
--
-- Note that this conduit /does not/ assume that the input is sorted. Instead
-- it uses a 'Data.Set' to store previously seen elements. Thus, memory usage
-- is O(N) and time is O(N log N). If the input is sorted, you can use
-- 'removeRepeatsC'
uniqueOnC :: (Ord b, Monad m) => (a -> b) -> C.ConduitT a a m ()
uniqueOnC f = checkU (S.empty :: S.Set b)
    where
        checkU cur = awaitJust $ \val ->
                        if f val `S.member` cur
                            then checkU cur
                            else do
                                C.yield val
                                checkU (S.insert (f val) cur)
-- | Unique conduit
--
-- See 'uniqueOnC' and 'removeRepeatsC'
uniqueC :: (Ord a, Monad m) => C.ConduitT a a m ()
uniqueC = uniqueOnC id

-- | Removes repeated elements
--
-- @
--  yieldMany [0, 0, 1, 1, 1, 2, 2, 0] .| removeRepeatsC .| consume
-- @
--
-- is equivalent to @[0, 1, 2, 0]@
--
-- See 'uniqueC' and 'uniqueOnC'
removeRepeatsC :: (Eq a, Monad m) => C.ConduitT a a m ()
removeRepeatsC = awaitJust removeRepeatsC'
    where
        removeRepeatsC' prev = C.await >>= \case
                                        Nothing -> C.yield prev
                                        Just next
                                            | next == prev -> removeRepeatsC' prev
                                            | otherwise -> do
                                                        C.yield prev
                                                        removeRepeatsC' next


-- | Merge a list of sorted sources to produce a single (sorted) source
--
-- This takes a list of sorted sources and produces a 'C.Source' which outputs
-- all elements in sorted order.
--
-- See 'mergeC2'
mergeC :: (Ord a, Monad m) => [C.ConduitT () a m ()] -> C.ConduitT () a m ()
mergeC [a] = a
mergeC [a,b] = mergeC2 a b
mergeC cs = CI.ConduitT $ \rest -> let
        go q = case PQ.minView q of
            Nothing -> rest()
            Just (CI.HaveOutput c_next v, q') -> CI.HaveOutput (norm1insert q' c_next >>= go)  v
            _ -> error "This situation should have been impossible (mergeC/go)"
        -- norm1insert inserts the pipe in into the queue after ensuring that the pipe is CI.HaveOutput
        norm1insert :: (Monad m, Ord o)
                            => PQ.PQueue o (CI.Pipe () i o () m ())
                            -> CI.Pipe () i o () m ()
                            -> CI.Pipe () i o () m (PQ.PQueue o (CI.Pipe () i o () m ()))
        norm1insert q c@(CI.HaveOutput _ v) = return (PQ.insert v c q)
        norm1insert q CI.Done{} = return q
        norm1insert q (CI.PipeM p) = lift p >>= norm1insert q
        norm1insert q (CI.NeedInput _ next) = norm1insert q (next ())
        norm1insert q (CI.Leftover next ()) = norm1insert q next
    in do
        let st = map (\c -> CI.unConduitT c $ CI.Done) cs
        go =<< foldM norm1insert PQ.empty st

-- | Take two sorted sources and merge them.
--
-- See 'mergeC'
mergeC2 :: (Ord a, Monad m) => C.ConduitT () a m () -> C.ConduitT () a m () -> C.ConduitT () a m ()
mergeC2 (CI.ConduitT s1) (CI.ConduitT s2) = CI.ConduitT $ \rest -> let
        go right@(CI.HaveOutput s1' v1) left@(CI.HaveOutput s2' v2)
            | v1 <= v2 = CI.HaveOutput (go s1' left) v1
            | otherwise = CI.HaveOutput (go right s2') v2
        go right@CI.Done{} (CI.HaveOutput s v) = CI.HaveOutput (go right s) v
        go (CI.HaveOutput s v) left@CI.Done{}  = CI.HaveOutput (go s left)  v
        go CI.Done{} CI.Done{} = rest ()
        go (CI.PipeM p) left = do
            next <- lift p
            go next left
        go right (CI.PipeM p) = do
            next <- lift p
            go right next
        go (CI.NeedInput _ next) left = go (next ()) left
        go right (CI.NeedInput _ next) = go right (next ())
        go (CI.Leftover next ()) left = go next left
        go right (CI.Leftover next ()) = go right next
    in go (s1 CI.Done) (s2 CI.Done)
