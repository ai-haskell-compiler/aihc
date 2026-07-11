module Data.Ord
  ( Ord (..),
    Ordering (..),
    comparing,
  )
where

import Prelude (Ord (..), Ordering (..))

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing projection x y = compare (projection x) (projection y)
