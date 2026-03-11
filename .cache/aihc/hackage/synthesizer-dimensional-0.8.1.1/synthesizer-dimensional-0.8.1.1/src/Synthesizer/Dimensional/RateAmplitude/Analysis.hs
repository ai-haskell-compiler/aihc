{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.RateAmplitude.Analysis (
    AnaR.centroid,
    AnaR.length,
    AnaA.beginning,
    AnaA.end,

    normMaximum,      normVectorMaximum,
    normEuclideanSqr, normVectorEuclideanSqr,
    normSum,          normVectorSum,

    normMaximumProc,      normVectorMaximumProc,
    normEuclideanSqrProc, normVectorEuclideanSqrProc,
    normSumProc,          normVectorSumProc,

    histogram,
    zeros,
  ) where

import qualified Synthesizer.State.Analysis as Ana
import qualified Synthesizer.State.Signal   as Sig

import qualified Synthesizer.Dimensional.Amplitude.Analysis   as AnaA
import qualified Synthesizer.Dimensional.Rate.Analysis        as AnaR
import qualified Synthesizer.Dimensional.Amplitude            as Amp
import qualified Synthesizer.Dimensional.Rate                 as Rate
import qualified Synthesizer.Dimensional.Process              as Proc
import qualified Synthesizer.Dimensional.Signal.Private       as SigA
import qualified Synthesizer.Dimensional.Rate.Dirac           as Dirac

import Synthesizer.Dimensional.Process (DimensionGradient, )

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((&*&), (*&), )

-- import qualified Number.Complex as Complex

import qualified Algebra.NormedSpace.Maximum   as NormedMax
import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.NormedSpace.Sum       as NormedSum

-- import qualified Algebra.Transcendental      as Trans
import qualified Algebra.Algebraic           as Algebraic
import qualified Algebra.RealField           as RealField
import qualified Algebra.Field               as Field
import qualified Algebra.RealRing            as RealRing
import qualified Algebra.Ring                as Ring
import qualified Algebra.Absolute            as Absolute


import NumericPrelude.Base (Ord, ($), (.), return, fmap, id, )
import NumericPrelude.Numeric (sqr, abs, )
import Prelude (Int, )


{- * Norms -}

type Signal u t v y yv =
   SigA.T (Rate.Dimensional u t) (Amp.Dimensional v y) (Sig.T yv)


{- |
Manhattan norm.
-}
{-# INLINE normMaximum #-}
normMaximum :: (RealRing.C y, Dim.C u, Dim.C v) =>
   Signal u t v y y -> DN.T v y
normMaximum =
   AnaA.volumeMaximum

{- |
Square of energy norm.

Could also be called @variance@.
-}
{-# INLINE normEuclideanSqr #-}
normEuclideanSqr :: (Algebraic.C q, Dim.C u, Dim.C v) =>
   Signal u q v q q ->
   DN.T (Dim.Mul u (Dim.Sqr v)) q
normEuclideanSqr =
   normAux DN.sqr (Sig.sum . Sig.map sqr)

{- |
Sum norm.
-}
{-# INLINE normSum #-}
normSum :: (Field.C q, Absolute.C q, Dim.C u, Dim.C v) =>
   Signal u q v q q ->
   DN.T (Dim.Mul u v) q
normSum =
   normAux id (Sig.sum . Sig.map abs)



{- |
Manhattan norm.
-}
{-# INLINE normVectorMaximum #-}
normVectorMaximum ::
   (NormedMax.C q yv, Ord q, Dim.C u, Dim.C v) =>
   Signal u q v q yv ->
   DN.T v q
normVectorMaximum =
   AnaA.volumeVectorMaximum -- NormedMax.norm

{- |
Energy norm.
-}
{-# INLINE normVectorEuclideanSqr #-}
normVectorEuclideanSqr ::
   (NormedEuc.C q yv, Algebraic.C q, Dim.C u, Dim.C v) =>
   Signal u q v q yv ->
   DN.T (Dim.Mul u (Dim.Sqr v)) q
normVectorEuclideanSqr =
   normAux DN.sqr (Sig.sum . Sig.map NormedEuc.normSqr)

{- |
Sum norm.
-}
{-# INLINE normVectorSum #-}
normVectorSum ::
   (NormedSum.C q yv, Field.C q, Dim.C u, Dim.C v) =>
   Signal u q v q yv ->
   DN.T (Dim.Mul u v) q
normVectorSum =
   normAux id (Sig.sum . Sig.map NormedSum.norm)


{-# INLINE normAux #-}
normAux :: (Dim.C v0, Dim.C v1, Dim.C u, Field.C t) =>
   (DN.T v0 y -> DN.T v1 t) ->
   (Sig.T yv -> t) ->
   Signal u t v0 y yv ->
   DN.T (Dim.Mul u v1) t
normAux amp norm x =
   norm (SigA.body x)
       *& DN.unrecip (SigA.actualSampleRate x)
      &*& amp (SigA.actualAmplitude x)




{-# DEPRECATED #-}
{- |
Manhattan norm.
-}
{-# INLINE normMaximumProc #-}
normMaximumProc :: (RealRing.C y, Dim.C u, Dim.C v) =>
   Proc.T s u y (SigA.R s v y y -> DN.T v y)
normMaximumProc =
   Proc.pure AnaA.volumeMaximum

{-# DEPRECATED #-}
{- |
Square of energy norm.

Could also be called @variance@.
-}
{-# INLINE normEuclideanSqrProc #-}
normEuclideanSqrProc :: (Algebraic.C q, Dim.C u, Dim.C v) =>
   Proc.T s u q (
      SigA.R s v q q ->
      DN.T (Dim.Mul u (Dim.Sqr v)) q)
normEuclideanSqrProc =
   normAuxProc DN.sqr (Sig.sum . Sig.map sqr)

{-# DEPRECATED #-}
{- |
Sum norm.
-}
{-# INLINE normSumProc #-}
normSumProc :: (Field.C q, Absolute.C q, Dim.C u, Dim.C v) =>
   Proc.T s u q (
      SigA.R s v q q ->
      DN.T (Dim.Mul u v) q)
normSumProc =
   normAuxProc id (Sig.sum . Sig.map abs)



{-# DEPRECATED #-}
{- |
Manhattan norm.
-}
{-# INLINE normVectorMaximumProc #-}
normVectorMaximumProc ::
   (NormedMax.C y yv, Ord y, Dim.C u, Dim.C v) =>
   Proc.T s u y (
      SigA.R s v y yv ->
      DN.T v y)
normVectorMaximumProc =
   Proc.pure AnaA.volumeVectorMaximum -- NormedMax.norm

{-# DEPRECATED #-}
{- |
Energy norm.
-}
{-# INLINE normVectorEuclideanSqrProc #-}
normVectorEuclideanSqrProc ::
   (NormedEuc.C y yv, Algebraic.C y, Dim.C u, Dim.C v) =>
   Proc.T s u y (
      SigA.R s v y yv ->
      DN.T (Dim.Mul u (Dim.Sqr v)) y)
normVectorEuclideanSqrProc =
   normAuxProc DN.sqr (Sig.sum . Sig.map NormedEuc.normSqr)

{-# DEPRECATED #-}
{- |
Sum norm.
-}
{-# INLINE normVectorSumProc #-}
normVectorSumProc ::
   (NormedSum.C y yv, Field.C y, Dim.C u, Dim.C v) =>
   Proc.T s u y (
      SigA.R s v y yv ->
      DN.T (Dim.Mul u v) y)
normVectorSumProc =
   normAuxProc id (Sig.sum . Sig.map NormedSum.norm)


{-# INLINE normAuxProc #-}
normAuxProc :: (Dim.C v0, Dim.C v1, Dim.C u, Field.C t) =>
   (DN.T v0 y -> DN.T v1 t) ->
   (Sig.T yv -> t) ->
   Proc.T s u t (
      SigA.R s v0 y yv ->
      DN.T (Dim.Mul u v1) t)
normAuxProc amp norm =
   Proc.withParam $ \ x ->
   fmap
      (&*& amp (SigA.actualAmplitude x))
      (Proc.toTimeDimension (norm (SigA.body x)))





{- * Miscellaneous -}

{-# INLINE histogram #-}
histogram :: (RealField.C q, Dim.C u, Dim.C v) =>
   Signal u q v q q ->
   Proc.T s v q (Int, SigA.R s (DimensionGradient v u) q q)
histogram xs =
   do rateY <- Proc.getSampleRate
      toTime <- Proc.withParam Proc.toTimeScalar
      return $
         let (offset, hist) =
                 Ana.histogramLinearIntMap
                    (SigA.scalarSamples toTime xs)
         in  (offset,
              SigA.fromBody
                 (rateY &*& DN.unrecip (SigA.actualSampleRate xs))
                 hist)

{- |
Detects zeros (sign changes) in a signal.
This can be used as a simple measure of the portion
of high frequencies or noise in the signal.
The result has a frequency as amplitude.
If you smooth it, you will get a curve that represents a frequency progress.
It ca be used as voiced\/unvoiced detector in a vocoder.

The result will be one value shorter than the input.
-}
{-# INLINE zeros #-}
zeros :: (Ord q, Ring.C q, Dim.C u, Dim.C v) =>
   Proc.T s u q (SigA.R s v q q -> SigA.R s (Dim.Recip u) q q)
zeros =
   fmap
      (\fp -> fp . Dirac.Cons . Ana.zeros . SigA.body)
      Dirac.toAmplitudeSignal
