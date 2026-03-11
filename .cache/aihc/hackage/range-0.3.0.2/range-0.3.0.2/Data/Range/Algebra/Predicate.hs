module Data.Range.Algebra.Predicate where

import Control.Applicative

import Data.Range.Algebra.Internal

predicateAlgebra :: Algebra RangeExprF (a -> Bool)
predicateAlgebra (Invert f)         = liftA not f
predicateAlgebra (Union f g)        = liftA2 (||) f g
predicateAlgebra (Intersection f g) = liftA2 (&&) f g
predicateAlgebra (Difference f g)   = liftA2 (&&~) f g
  where (&&~) a b = a && not b
