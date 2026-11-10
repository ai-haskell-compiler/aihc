module Aihc.Util.Occurrences
  ( count,
  )
where

import Data.Monoid (Sum)
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MonoidMap

-- | Count the occurrences of each element within a list.
count :: (Ord k, Integral n) => [k] -> MonoidMap k (Sum n)
count ks = MonoidMap.fromList [(k, 1) | k <- ks]
{-# INLINE count #-}
