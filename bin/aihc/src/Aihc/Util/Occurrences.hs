module Aihc.Util.Occurrences
  ( count,
  )
where

import Data.Monoid (Sum)
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MonoidMap

-- | Count how often each element of a list occurs.
count :: (Ord k, Integral n) => [k] -> MonoidMap k (Sum n)
count ks = MonoidMap.fromList [(k, 1) | k <- ks]
{-# INLINE count #-}
