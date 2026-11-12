module GHC.Num
  ( module GHC.Prim.Num,
    subtract,
  )
where

import GHC.Prim.Num

-- | Subtract the first argument from the second argument.
subtract :: (Num a) => a -> a -> a
subtract left right = right - left
