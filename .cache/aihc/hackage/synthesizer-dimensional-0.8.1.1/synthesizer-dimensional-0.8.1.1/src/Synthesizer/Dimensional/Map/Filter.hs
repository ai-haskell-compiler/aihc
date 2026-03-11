{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Copyright   :  (c) Henning Thielemann 2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Map.Filter (
   -- * Amplification
   amplify,
   amplifyDimension,
   amplifyScalarDimension,
   negate,
   envelope,
   envelopeScalarDimension,
   envelopeVector,
   envelopeVectorDimension,
 ) where

import qualified Synthesizer.Dimensional.Map as MapD
import qualified Synthesizer.Dimensional.Arrow as ArrowD
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Sample as Sample

import Control.Arrow (Arrow, )

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((&*&), )

-- import qualified Number.NonNegative     as NonNeg

-- import qualified Algebra.Transcendental as Trans
-- import qualified Algebra.RealRing      as RealRing
-- import qualified Algebra.Field          as Field
-- import qualified Algebra.Absolute           as Absolute
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
-- import qualified Algebra.VectorSpace    as VectorSpace
import qualified Algebra.Module         as Module

-- import Control.Monad(liftM2)

import NumericPrelude.Numeric hiding (negate)
import NumericPrelude.Base as P
import Prelude ()


{- | The amplification factor must be positive. -}
{-# INLINE amplify #-}
amplify ::
   (Module.C y amp, Arrow arrow) =>
   y ->
   ArrowD.Single arrow (Amp.Numeric amp) (Amp.Numeric amp) yv yv
amplify volume =
   MapD.independent (fmap (volume *>)) id

{-# INLINE amplifyDimension #-}
amplifyDimension ::
   (Ring.C y, Dim.C v0, Dim.C v1, Arrow arrow) =>
   DN.T v0 y ->
   ArrowD.Single arrow
      (Amp.Dimensional v1 y) (Amp.Dimensional (Dim.Mul v0 v1) y)
      yv yv
amplifyDimension volume =
   MapD.independent (fmap (volume &*&)) id

{-# INLINE amplifyScalarDimension #-}
amplifyScalarDimension ::
   (Ring.C y, Dim.C v, Arrow arrow) =>
   DN.T v y ->
   ArrowD.Single arrow
      (Amp.Dimensional Dim.Scalar y) (Amp.Dimensional v y)
      yv yv
amplifyScalarDimension volume =
   MapD.independent 
      (fmap $ flip DN.scale volume . DN.toNumber)
      id


{-# INLINE negate #-}
negate ::
   (Additive.C (Sample.Displacement sample), Arrow arrow) =>
   ArrowD.T arrow sample sample
negate =
   MapD.independent id Additive.negate


{-# INLINE envelope #-}
envelope ::
   (Ring.C y, Arrow arrow) =>
   ArrowD.T arrow (Sample.Flat y, Sample.Numeric amp y) (Sample.Numeric amp y)
envelope =
   MapD.independent snd (uncurry (*))

{-# INLINE envelopeScalarDimension #-}
envelopeScalarDimension ::
   (Ring.C y, Dim.C v, Arrow arrow) =>
   ArrowD.T arrow
      (Sample.Dimensional Dim.Scalar y y, Sample.Dimensional v y y)
      (Sample.Dimensional v y y)
envelopeScalarDimension =
   MapD.independent
      (\(Amp.Numeric ampEnv, Amp.Numeric ampSig) ->
         Amp.Numeric $ DN.scale (DN.toNumber ampEnv) ampSig)
      (uncurry (*))

{-# INLINE envelopeVector #-}
envelopeVector ::
   (Module.C y (Sample.Displacement sample), Arrow arrow) =>
   ArrowD.T arrow (Sample.Flat y, sample) sample
envelopeVector =
   MapD.independent snd (uncurry (*>))

{-# INLINE envelopeVectorDimension #-}
envelopeVectorDimension ::
   (Module.C y0 yv, Ring.C y, Dim.C v0, Dim.C v1, Arrow arrow) =>
   ArrowD.T arrow
      (Sample.Dimensional v0 y y0, Sample.Dimensional v1 y yv)
      (Sample.Dimensional (Dim.Mul v0 v1) y yv)
envelopeVectorDimension =
   MapD.independent
      (\(Amp.Numeric ampEnv, Amp.Numeric ampSig) ->
         Amp.Numeric $ ampEnv &*& ampSig)
      (uncurry (*>))
