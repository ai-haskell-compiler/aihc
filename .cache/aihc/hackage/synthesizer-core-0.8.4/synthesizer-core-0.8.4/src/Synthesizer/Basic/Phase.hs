module Synthesizer.Basic.Phase (
   T,
   fromRepresentative,
   toRepresentative,
   increment,
   decrement,
   multiply,
   ) where

import qualified Algebra.ToInteger             as ToInteger
import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import System.Random (Random(..))
import Test.QuickCheck (Arbitrary(arbitrary), choose)

import Foreign.Storable (Storable(..), )
import Foreign.Ptr (castPtr, )

import Data.Tuple.HT (mapFst, )

import qualified NumericPrelude.Numeric as NP
import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()

import qualified GHC.Float as GHC


newtype T a = Cons {decons :: a}
   deriving Eq


instance Show a => Show (T a) where
   showsPrec p x =
      showParen (p >= 10)
         (showString "Phase.fromRepresentative " . showsPrec 11 (toRepresentative x))

instance Storable a => Storable (T a) where
   {-# INLINE sizeOf #-}
   sizeOf = sizeOf . toRepresentative
   {-# INLINE alignment #-}
   alignment = alignment . toRepresentative
   {-# INLINE peek #-}
   peek ptr = fmap Cons $ peek (castPtr ptr)
   {-# INLINE poke #-}
   poke ptr = poke (castPtr ptr) . toRepresentative


instance (Ring.C a, Random a) => Random (T a) where
   randomR = error "Phase.randomR makes no sense"
   random = mapFst Cons . randomR (zero, one)

instance (Ring.C a, Random a) => Arbitrary (T a) where
   arbitrary = fmap Cons $ choose (zero, one)



{-# INLINE fromRepresentative #-}
fromRepresentative :: RealRing.C a => a -> T a
fromRepresentative = Cons . RealRing.fraction

{-# INLINE toRepresentative #-}
toRepresentative :: T a -> a
toRepresentative = decons

{- test, how fast the function can be, if we assume that the increment is smaller than one
{-# INLINE increment #-}
increment :: RealRing.C a => a -> T a -> T a
increment d = (+ Cons d)

{-# INLINE decrement #-}
decrement :: RealRing.C a => a -> T a -> T a
decrement d = Additive.subtract (Cons d)
-}

{-# INLINE increment #-}
increment :: RealRing.C a => a -> T a -> T a
increment d = lift (d Additive.+)

{-# INLINE decrement #-}
decrement :: RealRing.C a => a -> T a -> T a
decrement d = lift (Additive.subtract d)


{-# INLINE add #-}
add :: (Ring.C a, Ord a) => T a -> T a -> T a
add (Cons x) (Cons y) =
   let z = x+y
   in  Cons $ if z>=one then z-one else z

{-# INLINE sub #-}
sub :: (Ring.C a, Ord a) => T a -> T a -> T a
sub (Cons x) (Cons y) =
   let z = x-y
   in  Cons $ if z<zero then z+one else z

{-# INLINE neg #-}
neg :: (Ring.C a, Ord a) => T a -> T a
neg (Cons x) =
   Cons $ if x==zero then zero else one-x

{-# INLINE multiply #-}
multiply :: (RealRing.C a, ToInteger.C b) => b -> T a -> T a
multiply n = lift (NP.fromIntegral n Ring.*)

{-
This implementation computes the fraction several times.
We hope that it can reduce cancellations,
but interim rounding errors seem to be equally bad.

It is certainly slower than 'multiply' and
it needs as many iterations as the number of bits of the multiplier.

> *Synthesizer.Basic.Phase> multiplyPrecise  (1000000::Integer) (fromRepresentative 2.3) :: T Double
> Phase.fromRepresentative 0.9999999998223643
> *Synthesizer.Basic.Phase> multiply  (1000000::Integer) (fromRepresentative 2.3) :: T Double
> Phase.fromRepresentative 0.999999999825377

{-# INLINE multiplyPrecise #-}
multiplyPrecise :: (RealRing.C a, ToInteger.C b) => b -> T a -> T a
multiplyPrecise n x =
   if n<zero
     then powerAssociative (+) zero (neg x) (fromIntegral (negate n))
     else powerAssociative (+) zero x (fromIntegral n)
-}


instance RealRing.C a => Additive.C (T a) where
   {-# INLINE zero #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   {-# INLINE negate #-}
   zero = Cons Additive.zero
   (+) = add
   (-) = sub
   negate = neg
{-
This implementation requires fromRepresentative,
that needs to do checks on the size of numbers
in order to choose between float2Int/int2Float and Prelude.properFraction
   (+) = lift2 (Additive.+)
   (-) = lift2 (Additive.-)
   negate = lift Additive.negate
-}

{-# INLINE lift #-}
lift :: (RealRing.C b) =>
   (a -> b) -> T a -> T b
lift f =
   fromRepresentative . f . toRepresentative

{-
{-# INLINE lift2 #-}
lift2 :: (RealRing.C c) =>
   (a -> b -> c) -> T a -> T b -> T c
lift2 f x y =
   fromRepresentative (f (toRepresentative x) (toRepresentative y))
-}


{-# INLINE customFromRepresentative #-}
customFromRepresentative ::
   (Additive.C a) =>
   (a -> i) -> (i -> a) -> a -> T a
customFromRepresentative toInt fromInt x =
   Cons (x Additive.- fromInt (toInt x))

{-# INLINE customLift #-}
customLift ::
   (Additive.C b) =>
   (b -> i) -> (i -> b) ->
   (a -> b) -> T a -> T b
customLift toInt fromInt f =
   customFromRepresentative toInt fromInt . f . toRepresentative

{-
{-# INLINE customLift2 #-}
customLift2 ::
   (Additive.C c) =>
   (c -> i) -> (i -> c) ->
   (a -> b -> c) -> T a -> T b -> T c
customLift2 toInt fromInt f x y =
   customFromRepresentative toInt fromInt $
   f (toRepresentative x) (toRepresentative y)
-}

{-# INLINE customMultiply #-}
customMultiply ::
   (Ring.C a, Ord a, ToInteger.C b) =>
   (a -> i) -> (i -> a) ->
   b -> T a -> T a
customMultiply toInt fromInt n (Cons x) =
   customFromRepresentative toInt fromInt $
   if n<zero && x>zero
     then (one-x) * NP.fromIntegral (NP.negate n)
     else x * NP.fromIntegral n


{- |
Optimization for the case,
that the integral part of the number is non-negative and fits in an Int.
This is the case for addition and integral scaling.

FIXME:
The increment and decrement routines are a bit dangerous,
because they fail if the increment value is larger than maxBound::Int.
However, we will always use increments with absolute value below one.
-}
{-# RULES

     "Phase.multiply @ Float"  multiply = customMultiply GHC.float2Int  GHC.int2Float;
     "Phase.multiply @ Double" multiply = customMultiply GHC.double2Int GHC.int2Double;

     "Phase.increment @ Float"  increment = \d -> customLift GHC.float2Int  GHC.int2Float  (+d);
     "Phase.increment @ Double" increment = \d -> customLift GHC.double2Int GHC.int2Double (+d);

     "Phase.decrement @ Float"  decrement = \d -> customLift GHC.float2Int  GHC.int2Float  (subtract d);
     "Phase.decrement @ Double" decrement = \d -> customLift GHC.double2Int GHC.int2Double (subtract d);

  #-}

{-
     "Phase.+        @ Float"  (+) = customLift2 GHC.float2Int GHC.int2Float (+);
     "Phase.-        @ Float"  (-) = customLift2 GHC.float2Int GHC.int2Float (-);

     "Phase.+        @ Double" (+) = customLift2 GHC.double2Int GHC.int2Double (+);
     "Phase.-        @ Double" (-) = customLift2 GHC.double2Int GHC.int2Double (-);

-}
