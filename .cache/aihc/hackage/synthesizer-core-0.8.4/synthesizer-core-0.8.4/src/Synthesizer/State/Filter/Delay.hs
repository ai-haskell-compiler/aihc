{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.State.Filter.Delay (
   static,
   staticPad,
   staticPos,
   staticNeg,
   modulated,
   ) where

import qualified Synthesizer.Interpolation as Interpolation
import qualified Synthesizer.State.Interpolation as InterpolationS
import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.RealField as RealField
import qualified Algebra.Additive  as Additive

import NumericPrelude.Numeric



{- * Shift -}

{-# INLINE static #-}
static :: Additive.C y => Int -> Sig.T y -> Sig.T y
static = staticPad zero

{-# INLINE staticPad #-}
staticPad :: y -> Int -> Sig.T y -> Sig.T y
staticPad = InterpolationS.delayPad

{-# INLINE staticPos #-}
staticPos :: Additive.C y => Int -> Sig.T y -> Sig.T y
staticPos n = Sig.append (Sig.replicate n zero)

{-# INLINE staticNeg #-}
staticNeg :: Int -> Sig.T y -> Sig.T y
staticNeg = Sig.drop



{-# INLINE modulatedCore #-}
modulatedCore :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> Sig.T a -> Sig.T v -> Sig.T v
modulatedCore ip size =
   Sig.zipWithTails
      (\t -> InterpolationS.single ip (fromIntegral size + t))

{-
modulatedCoreSlow :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> Sig.T a -> Sig.T v -> Sig.T v
modulatedCoreSlow ip size ts xs =
   Sig.fromList $ zipWith
      (\t -> Interpolation.single ip (fromIntegral size - t))
      (Sig.toList ts) (Sig.tails xs)
-}

{- |
This is essentially different for constant interpolation,
because this function "looks forward"
whereas the other two variants "look backward".
For the symmetric interpolation functions
of linear and cubic interpolation, this does not really matter.
-}
{-# INLINE modulated #-}
modulated :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> Sig.T a -> Sig.T v -> Sig.T v
modulated ip minDev ts xs =
   let size = Interpolation.number ip - minDev
   in  modulatedCore ip
          (size - Interpolation.offset ip)
          ts
          (staticPos size xs)
