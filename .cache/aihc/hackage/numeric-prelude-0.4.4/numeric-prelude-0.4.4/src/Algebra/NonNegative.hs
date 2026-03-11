{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

A type class for non-negative numbers.
Prominent instances are 'Number.NonNegative.T' and 'Number.Peano.T' numbers.
This class cannot do any checks,
but it let you show to the user what arguments your function expects.
Thus you must define class instances with care.
In fact many standard functions ('take', '(!!)', ...)
should have this type class constraint.
-}
module Algebra.NonNegative (
   C(..),
   splitDefault,

   (-|),
--   (-?),
   zero,
   add,
   sum,
   ) where

import qualified Algebra.Additive as Additive

import qualified Algebra.Monoid as Monoid

import Algebra.Additive ((-), )

import Prelude hiding (sum, (-), abs, )


infixl 6 -|  -- , -?


{- |
Instances of this class must ensure non-negative values.
We cannot enforce this by types, but the type class constraint @NonNegative.C@
avoids accidental usage of types which allow for negative numbers.

The Monoid superclass contributes a zero and an addition.
-}
class (Ord a, Monoid.C a) => C a where
   {- |
   @split x y == (m,(b,d))@ means that
   @b == (x<=y)@,
   @m == min x y@,
   @d == max x y - min x y@, that is @d == abs(x-y)@.

   We have chosen this function as base function,
   since it provides comparison and subtraction in one go,
   which is important for replacing common structures like

   > if x<=y
   >   then f(x-y)
   >   else g(y-x)

   that lead to a memory leak for peano numbers.
   We have choosen the simple check @x<=y@
   instead of a full-blown @compare@,
   since we want @Zero <= undefined@ for peano numbers.
   Because of undefined values 'split' is in general
   not commutative in the sense

   > let (m0,(b0,d0)) = split x y
   >     (m1,(b1,d1)) = split y x
   > in  m0==m1 && d0==d1

   The result values are in the order
   in which they are generated for Peano numbers.
   We have chosen the nested pair instead of a triple
   in order to prevent a memory leak
   that occurs if you only use @b@ and @d@ and ignore @m@.
   This is demonstrated by test cases
   Chunky.splitSpaceLeak3 and Chunky.splitSpaceLeak4.
   -}
   split :: a -> a -> (a, (Bool, a))


{- |
Default implementation for wrapped types of 'Ord' and 'Num' class.
-}
{-# INLINE splitDefault #-}
splitDefault ::
   (Ord b, Additive.C b) =>
   (a -> b) -> (b -> a) -> a -> a -> (a, (Bool, a))
splitDefault unpack pack px py =
   let x = unpack px
       y = unpack py
   in  if x<=y
         then (pack x, (True,  pack (y-x)))
         else (pack y, (False, pack (x-y)))


zero :: C a => a
zero = Monoid.idt

-- like (+)
infixl 6 `add`

add :: C a => a -> a -> a
add = (Monoid.<*>)

sum :: C a => [a] -> a
sum = Monoid.cumulate


{- |
@x -| y == max 0 (x-y)@

The default implementation is not efficient,
because it compares the values and then subtracts, again, if safe.
@max 0 (x-y)@ is more elegant and efficient
but not possible in the general case,
since @x-y@ may already yield a negative number.
-}
(-|) :: C a => a -> a -> a
x -| y  =
   let (b,d) = snd $ split y x
   in  if b then d else zero

{-
(-?) :: (RealRing.C a) => a -> a -> (Bool, a)
(-?) x y  =  snd $ split y x
-}
