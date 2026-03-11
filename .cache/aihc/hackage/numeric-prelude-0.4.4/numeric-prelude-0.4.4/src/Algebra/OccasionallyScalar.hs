{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |

There are several types of numbers
where a subset of numbers can be considered as set of scalars.

 * A '(Complex.T Double)' value can be converted to 'Double' if the imaginary part is zero.

 * A value with physical units can be converted to a scalar if there is no unit. 

Of course this can be cascaded,
e.g. a complex number with physical units can be converted to a scalar
if there is both no imaginary part and no unit.

This is somewhat similar to the multi-type classes NormedMax.C and friends.

I hesitate to define an instance for lists
to avoid the mess known of MatLab.
But if you have an application where you think
you need this instance definitely
I'll think about that, again.

-}

module Algebra.OccasionallyScalar where

import Data.Maybe (fromMaybe, )

import NumericPrelude.Base
import NumericPrelude.Numeric


-- this is somehow similar to Normalized classes
class C a v where
   toScalar      :: v -> a
   toMaybeScalar :: v -> Maybe a
   fromScalar    :: a -> v

toScalarDefault :: (C a v) => v -> a
toScalarDefault v =
   fromMaybe (error ("The value is not scalar."))
             (toMaybeScalar v)

toScalarShow :: (C a v, Show v) => v -> a
toScalarShow v =
   fromMaybe (error (show v ++ " is not a scalar value."))
             (toMaybeScalar v)


instance C Float Float where
   toScalar      = id
   toMaybeScalar = Just
   fromScalar    = id

instance C Double Double where
   toScalar      = id
   toMaybeScalar = Just
   fromScalar    = id

{- converting values automatically to integers is a bad idea
instance (Integral b, RealRing.C a)
      => C b a where
   toScalar        = toScalarDefault
   toMaybeScalar x = mapMaybe round (toMaybeScalar x)
-}
