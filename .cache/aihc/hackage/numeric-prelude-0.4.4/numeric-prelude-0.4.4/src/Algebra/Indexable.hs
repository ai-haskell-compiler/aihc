{- |
Copyright    :   (c) Henning Thielemann 2007
Maintainer   :   numericprelude@henning-thielemann.de
Stability    :   provisional
Portability  :   portable

An alternative type class for Ord
which allows an ordering for dictionaries like "Data.Map" and "Data.Set"
independently from the ordering with respect to a magnitude.
-}

module Algebra.Indexable (
    C(compare),
    ordCompare,
    liftCompare,
    ToOrd,
    toOrd,
    fromOrd,
    ) where

import Prelude hiding (compare)

import qualified Prelude as P


{- |
Definition of an alternative ordering of objects
independent from a notion of magnitude.
For an application see "MathObj.PartialFraction".
-}
class Eq a => C a where
   compare :: a -> a -> Ordering

{- |
If the type has already an 'Ord' instance
it is certainly the most easiest to define 'Algebra.Indexable.compare'
to be equal to @Ord@'s 'compare'.
-}
ordCompare :: Ord a => a -> a -> Ordering
ordCompare = P.compare

{- |
Lift 'compare' implementation from a wrapped object.
-}
liftCompare :: C b => (a -> b) -> a -> a -> Ordering
liftCompare f x y = compare (f x) (f y)


instance (C a, C b) => C (a,b) where
   compare (x0,x1) (y0,y1) =
      let res = compare x0 y0
      in  case res of
             EQ -> compare x1 y1
             _  -> res

instance C a => C [a] where
   compare [] [] = EQ
   compare [] _  = LT
   compare _  [] = GT
   compare (x:xs) (y:ys) = compare (x,xs) (y,ys)

instance C Integer where
   compare = ordCompare


{- |
Wrap an indexable object such that it can be used in "Data.Map" and "Data.Set".
-}
newtype ToOrd a = ToOrd {fromOrd :: a} deriving (Eq, Show)

toOrd :: a -> ToOrd a
toOrd = ToOrd


instance C a => Ord (ToOrd a) where
   compare (ToOrd x) (ToOrd y) = compare x y
