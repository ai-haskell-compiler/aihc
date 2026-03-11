{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.RateAmplitude.Displacement (
   mix, mixVolume,
   mixMulti, mixMultiVolume,
   raise, raiseVector, distort,
   ) where

import qualified Synthesizer.Dimensional.Amplitude.Displacement as DispV

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import qualified Algebra.Module         as Module
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute           as Absolute
-- import qualified Algebra.Ring           as Ring
-- import qualified Algebra.Additive       as Additive

-- import Algebra.Module ((*>))

import NumericPrelude.Base
-- import NumericPrelude.Numeric
import Prelude ()


{- * Mixing -}

{-| Mix two signals.
    In opposition to 'zipWith' the result has the length of the longer signal. -}
{-# INLINE mix #-}
mix :: (Absolute.C y, Field.C y, Module.C y yv, Dim.C v) =>
      Proc.T s u t (
        SigA.R s v y yv
     -> SigA.R s v y yv
     -> SigA.R s v y yv)
mix = Proc.pure DispV.mix

{-# INLINE mixVolume #-}
mixVolume ::
   (Absolute.C y, Field.C y, Module.C y yv, Dim.C v) =>
      DN.T v y
   -> Proc.T s u t (
        SigA.R s v y yv
     -> SigA.R s v y yv
     -> SigA.R s v y yv)
mixVolume v = Proc.pure $ DispV.mixVolume v

{- |
Mix one or more signals.
-}
{-# INLINE mixMulti #-}
mixMulti ::
   (Absolute.C y, Field.C y, Module.C y yv, Dim.C v) =>
      Proc.T s u t (
        [SigA.R s v y yv]
     ->  SigA.R s v y yv)
mixMulti = Proc.pure DispV.mixMulti

{-# INLINE mixMultiVolume #-}
mixMultiVolume ::
   (Absolute.C y, Field.C y, Module.C y yv, Dim.C v) =>
      DN.T v y
   -> Proc.T s u t (
        [SigA.R s v y yv]
     ->  SigA.R s v y yv)
mixMultiVolume v = Proc.pure $ DispV.mixMultiVolume v

{- |
Add a number to all of the signal values.
This is useful for adjusting the center of a modulation.
-}
{-# INLINE raise #-}
raise :: (Field.C y, Dim.C v) =>
      DN.T v y
   -> Proc.T s u t (
        SigA.R s v y y
     -> SigA.R s v y y)
raise y' = Proc.pure $ DispV.raise y'

{-# INLINE raiseVector #-}
raiseVector :: (Field.C y, Module.C y yv, Dim.C v) =>
      DN.T v y
   -> yv
   -> Proc.T s u t (
        SigA.R s v y yv
     -> SigA.R s v y yv)
raiseVector y' yv = Proc.pure $ DispV.raiseVector y' yv

{- |
Distort the signal using a flat function.
The first signal gives the scaling of the function.
If the scaling is c and the input sample is y,
then @c * f(y/c)@ is output.
This way we can use an (efficient) flat function
and have a simple, yet dimension conform, way of controlling the distortion.
E.g. if the distortion function is @tanh@
then the value @c@ controls the saturation level.
-}
{-# INLINE distort #-}
distort :: (Field.C y, Module.C y yv, Dim.C v) =>
      (yv -> yv)
   -> Proc.T s u t (
        SigA.R s v y y
     -> SigA.R s v y yv
     -> SigA.R s v y yv)
distort f = Proc.pure $ DispV.distort f



{- convert values to different graduations

{- |
Map a control curve without amplitude unit
by a linear (affine) function with a unit.
-}
{-# INLINE mapLinearDimension #-}
mapLinearDimension :: (Field.C y, Absolute.C y, Dim.C u, Dim.C v) =>
      DN.T v y              {- ^ range: one is mapped to @center + range * ampX@ -}
   -> DN.T (Dim.Mul v u) y  {- ^ center: zero is mapped to @center@ -}
   -> Proc.T s u t (
        SigA.R s u y y
     -> SigA.R s (Dim.Mul v u) y y)
mapLinearDimension range center =
   Proc.pure $ CtrlA.mapLinearDimension range center

{- |
Map a control curve without amplitude unit
exponentially to one with a unit.
-}
{-# INLINE mapExponentialDimension #-}
mapExponentialDimension :: (Trans.C y, Dim.C u) =>
      y         {- ^ range: one is mapped to @center*range@, must be positive -}
   -> DN.T u y  {- ^ center: zero is mapped to @center@ -}
   -> Proc.T s u t (
        SigA.R s Dim.Scalar y y
     -> SigA.R s u y y)
mapExponentialDimension range center =
   Proc.pure $ CtrlA.mapExponential range center
-}
