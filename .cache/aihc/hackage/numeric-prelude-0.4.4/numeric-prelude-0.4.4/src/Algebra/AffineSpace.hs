{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
This module is not yet exported
since its interface is not mature.
There are two approaches for representing affine spaces:

[1] Two sets: A set of points and a set of vectors.
    Examples: Absolute potential and voltage,
    absolute temperature and temperature difference.
    Operations are
      add :: vector -> point -> point
      sub :: point -> point -> vector

[2] One set for points, no vectors.
    Examples: Interpolation
    Operation:
      combine :: [(coefficient, vector)] -> vector
    Where it must be asserted,
    that the coefficients sum up to 1.

The second one is the one we follow here.
It is more similar to Module and VectorSpace.
-}
module Algebra.AffineSpace where

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module
import qualified Number.Ratio as Ratio

import qualified Number.Complex as Complex

import Control.Applicative (Applicative(pure, (<*>)), )

import NumericPrelude.Numeric hiding (zero, )
import NumericPrelude.Base
import Prelude ()

{- |
The type class is for representing affine spaces via affine combinations.
However, we didn't find a way to both ensure the property
that the combination coefficients sum up to 1,
and keep it efficient.

We propose this class instead of a combination of Additive and Module
for interpolation for those types,
where scaling and addition alone makes no sense.
Such types are e.g. internal filter parameters in signal processing:
For these types interpolation makes definitely sense,
but addition and scaling make not.

That is, both classes are isomorphic
(you can define one in terms of the other),
but instances of this class are more easily defined,
and using an AffineSpace constraint instead of Module in a type signature
is important for documentation purposes.
AffineSpace should be superclass of Module.
(But then you may ask, why not adding another superclass for Convex spaces.
This class would provide a linear combination operation,
where the coefficients sum up to one
and all of them are non-negative.)

We may add a safety layer that ensures
that the coefficients sum up to 1,
using start points on the simplex
and functions to move on the simplex.
Start points have components that sum up to 1, e.g.

> (1, 0, 0, 0)
> (0, 1, 0, 0)
> (0, 0, 1, 0)
> (0, 0, 0, 1)
> (1/4, 1/4, 1/4, 1/4)

Then you may move along the simplex in the directions

> (1,  -1, 0,  0)
> (0,   1, 0, -1)
> (-1, -1, 3, -1)

which are characterized by components that sum up to 0.

For example linear combination is defined by

> lerp k (a,b) = (1-k)*>a + k*>b

that is the coefficients are (1-k) and k.
The pair (1-k, k) can be constructed
by starting at pair (1,0)
and moving k units in direction (-1,1).

> (1-k, k) = (1,0) + k*(-1,1)

It is however a challenge to manage the coefficient tuples
in a type safe and efficient way.
For small numbers of interpolation nodes
(constant, linear, cubic interpolation)
a type level list would appropriate,
but what to do for large tuples
like for Whittaker interpolation?


As side note:
In principle it would be sufficient
to provide an affine combination of two points,
since all affine combinations of more points
can be decomposed into such simple combinations.

> lerp a x y = (1-a)*>x + a*>y

E.g. @a*>x + b*>y + c*>z@ with @a+b+c=1@
can be written as @lerp c (lerp (b/(1-c)) x y) z@.
More generally you can use

> lerpnorm a b x y
>    = lerp (b/(a+b)) x y
>    = (a/(a+b))*>x + (b/(a+b))*>y

for writing

> a*>x + b*>y + c*>z ==
>    lerpnorm (a+b) c (lerpnorm a b x y) z

or

> a*>x + b*>y + c*>z + d*>w ==
>    lerpnorm (a+b+c) d (lerpnorm (a+b) c (lerpnorm a b x y) z) w

with @a+b+c+d=1@.

The downside is, that lerpnorm requires division, that is, a field,
whereas the computation of the coefficients
sometimes only requires ring operations.
-}
class Zero v => C a v where
   multiplyAccumulate :: (a,v) -> v -> v

class Zero v where
   zero :: v


instance Zero Float where
   {-# INLINE zero #-}
   zero = Additive.zero

instance Zero Double where
   {-# INLINE zero #-}
   zero = Additive.zero

instance (Zero a) => Zero (Complex.T a) where
   {-# INLINE zero #-}
   zero = zero Complex.+: zero

instance (PID.C a) => Zero (Ratio.T a) where
   {-# INLINE zero #-}
   zero = Additive.zero


instance C Float Float where
   {-# INLINE multiplyAccumulate #-}
   multiplyAccumulate (a,x) y = a*x+y

instance C Double Double where
   {-# INLINE multiplyAccumulate #-}
   multiplyAccumulate (a,x) y = a*x+y

instance (C a v) => C a (Complex.T v) where
   {-# INLINE multiplyAccumulate #-}
   multiplyAccumulate =
      makeMac2 (Complex.+:) Complex.real Complex.imag

instance (PID.C a) => C (Ratio.T a) (Ratio.T a) where
   {-# INLINE multiplyAccumulate #-}
   multiplyAccumulate (a,x) y = a*x+y


infixl 6 *.+

{- |
Infix variant of 'multiplyAccumulate'.
-}
{-# INLINE (*.+) #-}
(*.+) :: C a v => v -> (a,v) -> v
(*.+) = flip multiplyAccumulate


-- * convenience functions for defining multiplyAccumulate

{-# INLINE multiplyAccumulateModule #-}
multiplyAccumulateModule ::
   Module.C a v =>
   (a,v) -> v -> v
multiplyAccumulateModule (a,x) y =
   a *> x + y


{- |
A special reader monad.
-}
newtype MAC a v x = MAC {runMac :: (a,v) -> v -> x}

{-# INLINE element #-}
element ::
   (C a x) =>
   (v -> x) -> MAC a v x
element f =
   MAC (\(a,x) y -> multiplyAccumulate (a, f x) (f y))

instance Functor (MAC a v) where
   {-# INLINE fmap #-}
   fmap f (MAC x) =
      MAC $ \av v -> f $ x av v

instance Applicative (MAC a v) where
   {-# INLINE pure #-}
   {-# INLINE (<*>) #-}
   pure x = MAC $ \ _av _v -> x
   MAC f <*> MAC x =
      MAC $ \av v -> f av v $ x av v

{-# INLINE makeMac #-}
makeMac ::
   (C a x) =>
   (x -> v) ->
   (v -> x) ->
   (a,v) -> v -> v
makeMac cons x =
   runMac $ pure cons <*> element x

{-# INLINE makeMac2 #-}
makeMac2 ::
   (C a x, C a y) =>
   (x -> y -> v) ->
   (v -> x) -> (v -> y) ->
   (a,v) -> v -> v
makeMac2 cons x y =
   runMac $ pure cons <*> element x <*> element y

{-# INLINE makeMac3 #-}
makeMac3 ::
   (C a x, C a y, C a z) =>
   (x -> y -> z -> v) ->
   (v -> x) -> (v -> y) -> (v -> z) ->
   (a,v) -> v -> v
makeMac3 cons x y z =
   runMac $ pure cons <*> element x <*> element y <*> element z
