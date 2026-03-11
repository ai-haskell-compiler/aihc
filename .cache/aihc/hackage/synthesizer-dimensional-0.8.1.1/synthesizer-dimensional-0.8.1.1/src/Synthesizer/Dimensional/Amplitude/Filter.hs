{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Amplitude.Filter (
   {- * Non-recursive -}

   {- ** Amplification -}
   amplify,
   amplifyDimension,
   amplifyScalarDimension,
   negate,
   envelope,
   envelopeScalarDimension,
   envelopeVector,
   envelopeVectorDimension,
 ) where


import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat

-- import qualified Synthesizer.Dimensional.Straight.Signal      as SigS
import qualified Synthesizer.Dimensional.Signal.Private as SigA
-- import Synthesizer.Dimensional.Signal.Private (toAmplitudeScalar)

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((&*&))

import qualified Synthesizer.Generic.Signal            as SigG
import qualified Synthesizer.State.Signal              as Sig
import qualified Synthesizer.State.Filter.NonRecursive as FiltNR

-- import qualified Algebra.Transcendental as Trans
-- import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
import qualified Algebra.Module         as Module

-- import NumericPrelude.Numeric hiding (negate)
-- import NumericPrelude.Base as P
import Prelude ((.), flip, fmap, )


{- | The amplification factor must be positive. -}
{-# INLINE amplify #-}
amplify :: (Ring.C y, Dim.C u) =>
   y ->
   SigA.T rate (Amp.Dimensional u y) body ->
   SigA.T rate (Amp.Dimensional u y) body
amplify volume =
   processAmplitude (DN.scale volume)

{-# INLINE amplifyDimension #-}
amplifyDimension :: (Ring.C y, Dim.C u, Dim.C v) =>
   DN.T v y ->
   SigA.T rate (Amp.Dimensional u y) body ->
   SigA.T rate (Amp.Dimensional (Dim.Mul v u) y) body
amplifyDimension volume =
   processAmplitude (volume &*&)

{-# INLINE amplifyScalarDimension #-}
amplifyScalarDimension :: (Ring.C y, Dim.C v) =>
   DN.T v y ->
   SigA.T rate (Amp.Dimensional Dim.Scalar y) body ->
   SigA.T rate (Amp.Dimensional v y) body
amplifyScalarDimension volume =
   processAmplitude (flip DN.scale volume . DN.toNumber)

processAmplitude ::
   (amp0 -> amp1) ->
   SigA.T rate (Amp.Numeric amp0) body ->
   SigA.T rate (Amp.Numeric amp1) body
processAmplitude f (SigA.Cons rate amp xs) =
   SigA.Cons rate (fmap f amp) xs

-- FIXME: move to Dimensional.Straight
{-# INLINE negate #-}
negate :: (SigG.Transform sig yv, Additive.C yv) =>
      SigA.T rate amp (sig yv)
   -> SigA.T rate amp (sig yv)
negate =
   SigA.processBody (SigG.map Additive.negate)

-- FIXME: move to Dimensional.Straight
{-# INLINE envelope #-}
envelope :: (Flat.C y flat, Ring.C y) =>
      SigA.T (Rate.Phantom s) flat (Sig.T y)   {- ^ the envelope -}
   -> SigA.T (Rate.Phantom s) amp (Sig.T y)    {- ^ the signal to be enveloped -}
   -> SigA.T (Rate.Phantom s) amp (Sig.T y)
envelope y =
   SigA.processBody (FiltNR.envelope (Flat.toSamples y))

{- |
This is like 'envelope' but it does not require
prior conversion to a flat signal,
what might violate the sample range (-1,1).
Instead the global amplitudes are multiplied.
-}
{-# INLINE envelopeScalarDimension #-}
envelopeScalarDimension :: (Dim.C v, Ring.C y) =>
      SigA.R s Dim.Scalar y y
         {- ^ the envelope -}
   -> SigA.R s v y y
         {- ^ the signal to be enveloped -}
   -> SigA.R s v y y
envelopeScalarDimension y =
   processAmplitude (DN.scale (DN.toNumber (SigA.actualAmplitude y))) .
   SigA.processBody (FiltNR.envelope (SigA.body y))

-- FIXME: move to Dimensional.Straight
{-# INLINE envelopeVector #-}
envelopeVector :: (Flat.C y0 flat, Module.C y0 yv) =>
      SigA.T (Rate.Phantom s) flat (Sig.T y0)   {- ^ the envelope -}
   -> SigA.T (Rate.Phantom s) amp (Sig.T yv)    {- ^ the signal to be enveloped -}
   -> SigA.T (Rate.Phantom s) amp (Sig.T yv)
envelopeVector y =
   SigA.processBody (FiltNR.envelopeVector (Flat.toSamples y))

{-# INLINE envelopeVectorDimension #-}
envelopeVectorDimension :: (Module.C y0 yv, Ring.C y, Dim.C u, Dim.C v) =>
      SigA.R s v y y0  {- ^ the envelope -}
   -> SigA.R s u y yv  {- ^ the signal to be enveloped -}
   -> SigA.R s (Dim.Mul v u) y yv
envelopeVectorDimension y x =
   SigA.fromBody
      (SigA.actualAmplitude y &*& SigA.actualAmplitude x)
      (FiltNR.envelopeVector (SigA.body y) (SigA.body x))
