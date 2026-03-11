module TypeFun.Constraint
  ( AllSatisfy
  ) where

import GHC.Exts

-- | Apply constraint for each element of list
type family AllSatisfy (c :: k -> Constraint) (s :: [k]) :: Constraint where
  AllSatisfy c '[]       = ()
  AllSatisfy c (a ': as) = (c a, AllSatisfy c as)
