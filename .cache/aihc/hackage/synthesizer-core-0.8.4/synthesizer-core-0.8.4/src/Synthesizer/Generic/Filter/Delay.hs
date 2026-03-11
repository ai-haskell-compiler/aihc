{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Generic.Filter.Delay (
   static,
   staticPad,
   staticPos,
   staticNeg,
   modulated,
   ) where

import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNR
import qualified Synthesizer.Generic.Interpolation as Interpolation
import qualified Synthesizer.Generic.Signal  as SigG

import qualified Algebra.RealField as RealField
import qualified Algebra.Additive  as Additive

import NumericPrelude.Numeric



{- * Shift -}

{-# INLINE static #-}
static ::
   (Additive.C y, SigG.Write sig y) =>
   Int -> sig y -> sig y
static = FiltNR.delay

{-# INLINE staticPad #-}
staticPad ::
   (SigG.Write sig y) =>
   y -> Int -> sig y -> sig y
staticPad = FiltNR.delayPad

{-# INLINE staticPos #-}
staticPos ::
   (Additive.C y, SigG.Write sig y) =>
   Int -> sig y -> sig y
staticPos = FiltNR.delayPos

{-# INLINE staticNeg #-}
staticNeg ::
   (SigG.Write sig y) =>
   Int -> sig y -> sig y
staticNeg = FiltNR.delayNeg




{-# INLINE modulatedCore #-}
modulatedCore ::
   (RealField.C t, Additive.C y, SigG.Read sig t, SigG.Transform sig t, SigG.Transform sig y) =>
   Interpolation.T t y -> Int ->
   sig t -> sig y -> sig y
modulatedCore ip size =
   SigG.zipWithTails
      (\t -> Interpolation.single ip (fromIntegral size + t))


{- |
This is essentially different for constant interpolation,
because this function "looks forward"
whereas the other two variants "look backward".
For the symmetric interpolation functions
of linear and cubic interpolation, this does not really matter.
-}
{-# INLINE modulated #-}
modulated ::
   (RealField.C t, Additive.C y,
    SigG.Read sig t, SigG.Transform sig t, SigG.Transform sig y, SigG.Write sig y) =>
   Interpolation.T t y -> Int ->
   sig t -> sig y -> sig y
modulated ip minDev ts xs =
   let size = Interpolation.number ip - minDev
   in  modulatedCore ip
          (size - Interpolation.offset ip)
          ts
          (staticPos size xs)
