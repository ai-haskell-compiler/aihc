{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Basic.Binary (
   C(..), toCanonical,
   fromCanonicalWith, fromCanonicalSimpleWith,
   numberOfSignalChannels,
   int16ToCanonical, int16FromCanonical,
   int16FromFloat, int16FromDouble,
   ) where

import qualified Synthesizer.Frame.Stereo as Stereo

import Data.Monoid (Monoid, mappend, )

import qualified Algebra.FloatingPoint as Float
import qualified Algebra.ToInteger as ToInteger
import qualified Algebra.RealField as RealField
import qualified Algebra.RealRing  as RealRing
import qualified Algebra.Field     as Field
import qualified Algebra.Ring      as Ring

import Data.Ord.HT (limit, )
import Data.Tuple.HT (mapFst, )

import Data.Int (Int16, )
import GHC.Float (float2Int, double2Int, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import qualified Prelude as P98



class C a where
   outputFromCanonical ::
      (Bounded int, ToInteger.C int, Monoid out) =>
      (int -> out) -> a -> out
   numberOfChannels :: a -> Int

instance C Float where
   outputFromCanonical pack =
      pack .
      fromCanonicalWith
         (fromIntegral . truncToRound float2Int)
   numberOfChannels _ = 1

instance C Double where
   outputFromCanonical pack =
      pack .
      fromCanonicalWith
         (fromIntegral . truncToRound double2Int)
   numberOfChannels _ = 1

instance (C a, C b) => C (a,b) where
   outputFromCanonical pack x =
      outputFromCanonical pack (fst x) `mappend`
      outputFromCanonical pack (snd x)
   numberOfChannels x =
      numberOfChannels (fst x) +
      numberOfChannels (snd x)

instance (C a) => C (Stereo.T a) where
   outputFromCanonical pack x =
      outputFromCanonical pack (Stereo.left x) `mappend`
      outputFromCanonical pack (Stereo.right x)
   numberOfChannels x =
      numberOfChannels (Stereo.left x) +
      numberOfChannels (Stereo.right x)



{-# INLINE numberOfSignalChannels #-}
numberOfSignalChannels ::
   C yv => sig yv -> Int
numberOfSignalChannels sig =
   let aux :: C yv => sig yv -> yv -> Int
       aux _ dummy = numberOfChannels dummy
   in  aux sig undefined

{-# INLINE fromCanonicalWith #-}
fromCanonicalWith ::
   (Float.C real, Bounded int, ToInteger.C int) =>
   (real -> int) -> (real -> int)
fromCanonicalWith rnd r =
   let {-
       I hope that the complicated computation of a proper upper bound
       is turned into a constant.
       -}
       s = fromIntegral (maxBound `asTypeOf` i)
       {-
       The floating point type might be less precise
       than the integer type.
       In this case the upper bound might be rounded up
       when converting from integer to float.
       Then converting back from float to integer
       may yield a negative value.
       -}
       ss = if rnd s < 0 then decreaseFloat s else s
       i = rnd (ss * limit (-1, 1) r)
   in  i

{-# INLINE decreaseFloat #-}
decreaseFloat :: Float.C a => a -> a
decreaseFloat =
   uncurry Float.encode . mapFst (subtract 1) . Float.decode


{- |
Warning:
This may produce negative results for positive input in some cases!
The problem is that (maxBound :: Int32) cannot be represented exactly as Float,
the Float value is actually a bit larger than the Int32 value.
Thus when converting the Float back to Int32 it becomes negative.
Better use 'fromCanonicalWith'.
-}
{-# INLINE fromCanonicalSimpleWith #-}
fromCanonicalSimpleWith ::
   (RealRing.C real, Bounded int, ToInteger.C int) =>
   (real -> int) -> (real -> int)
fromCanonicalSimpleWith rnd r =
   let s = fromIntegral (maxBound `asTypeOf` i)
       i = rnd (s * limit (-1, 1) r)
   in  i

{-# INLINE truncToRound #-}
truncToRound ::
   (RealField.C real) =>
   (real -> int) -> (real -> int)
truncToRound trunc x =
   trunc $
   if x<0
     then x - 0.5
     else x + 0.5

{-# INLINE scale16 #-}
scale16 :: (Ring.C a, Ord a) => a -> a
scale16 x = 32767 * limit (-1, 1) x

{-# INLINE int16FromCanonical #-}
int16FromCanonical :: (RealRing.C a) => a -> Int16
{-
The round procedure is complicated and usually unnecessary
int16FromCanonical = (P98.fromIntegral :: Int -> Int16) . round . scale16
-}
{- in GHC-6.4 inefficient, since 'round' for target Int16 is not optimized
int16FromCanonical = round . scale16
-}
int16FromCanonical =
   (P98.fromIntegral :: Int -> Int16) . RealRing.roundSimple . scale16

{-# INLINE int16FromFloat #-}
int16FromFloat :: Float -> Int16
int16FromFloat = P98.fromIntegral . float2Int . scale16


{-
{-# INLINE scale16Double #-}
scale16Double :: (Ring.C a, Ord a) => a -> a
scale16Double x = 32767 * clip (-1) 1 x
-}

{-# INLINE int16FromDouble #-}
int16FromDouble :: Double -> Int16
{- Why is scale16 not inlined here? See FusionTest.mixTest3
int16FromDouble = P98.fromIntegral . double2Int . scale16
-}
-- int16FromDouble = P98.fromIntegral . double2Int . scale16Double
-- int16FromDouble x = P98.fromIntegral (double2Int (scale16 x))
int16FromDouble = P98.fromIntegral . double2Int . (32767*) . limit (-1, 1)




{-# INLINE toCanonical #-}
toCanonical ::
   (Field.C real, Bounded int, ToInteger.C int) =>
   (int -> real)
toCanonical i =
   let s = fromIntegral (maxBound `asTypeOf` i)
   in  fromIntegral i / s

{-# INLINE int16ToCanonical #-}
int16ToCanonical :: (Field.C a) => Int16 -> a
int16ToCanonical x = fromIntegral x / 32767
