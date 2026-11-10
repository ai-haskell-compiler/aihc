{-# LANGUAGE RankNTypes #-}

module Aihc.Util.Occurrences
  ( count,
    countST,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum)
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MonoidMap
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)

-- | Count the occurrences of each element within a list.
count :: (Ord k, Integral n) => [k] -> MonoidMap k (Sum n)
count ks = MonoidMap.fromList [(k, 1) | k <- ks]
{-# INLINE count #-}

-- | Count the occurrences of each element yielded by a computation.
--
-- This function uses in-place updates in order to minimise memory allocation.
--
-- A new element-count pair is added to the result map only when an element is
-- encountered for the first time. For all subsequent encounters of the same
-- element, the existing count is updated in place.
countST ::
  (Ord k, Integral n) =>
  (forall s. (k -> ST s ()) -> ST s ()) ->
  MonoidMap k (Sum n)
countST walk = MonoidMap.fromMap $ runST $ do
  table <- newSTRef Map.empty
  walk $ \k -> do
    counters <- readSTRef table
    case Map.lookup k counters of
      Just counter -> modifySTRef' counter (+ 1)
      Nothing -> do
        counter <- newSTRef 1
        writeSTRef table $! Map.insert k counter counters
  traverse readSTRef =<< readSTRef table
{-# INLINE countST #-}
