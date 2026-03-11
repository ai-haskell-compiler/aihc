{-# LANGUAGE FlexibleContexts #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2011
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Cyclic.Analysis (
    toFrequencySpectrum, fromFrequencySpectrum,
  ) where

import qualified Synthesizer.Generic.Fourier as FourierG
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG

import qualified Synthesizer.Dimensional.Rate                 as Rate
import qualified Synthesizer.Dimensional.Amplitude            as Amp
import qualified Synthesizer.Dimensional.Signal.Private       as SigA
import qualified Synthesizer.Dimensional.Cyclic.Signal        as SigC

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((&*&), (*&), )

import qualified Number.Complex as Complex

import qualified Algebra.Transcendental      as Trans
import qualified Algebra.Field               as Field


import NumericPrelude.Base ((.), ($), )
import NumericPrelude.Numeric (fromIntegral, )
import Prelude ()


{- * Positions -}

{-# INLINE period #-}
period :: (Field.C t, Dim.C u, CutG.Read body) =>
   SigA.T (Rate.Dimensional u t) amp (SigC.T body) ->
   DN.T u t
period = makePhysicalPeriod (fromIntegral . CutG.length)

{-# INLINE makePhysicalPeriod #-}
makePhysicalPeriod :: (Field.C t, Dim.C u) =>
   (body -> t) ->
   SigA.T (Rate.Dimensional u t) amp (SigC.T body) ->
   DN.T u t
makePhysicalPeriod f x =
   f (SigC.toPeriod (SigA.body x))
       *&  DN.unrecip (SigA.actualSampleRate x)


{- |
Fourier analysis
-}
{-# INLINE toFrequencySpectrum #-}
toFrequencySpectrum ::
   (Trans.C q, Dim.C u, Dim.C v,
    SigG.Transform sig (Complex.T q)) =>
   SigA.T (Rate.Dimensional u q) (Amp.Dimensional v q) (SigC.T (sig (Complex.T q))) ->
   SigA.T (Rate.Dimensional (Dim.Recip u) q) (Amp.Dimensional (Dim.Mul u v) q) (SigC.T (sig (Complex.T q)))
toFrequencySpectrum x =
   let len = DN.rewriteDimension Dim.doubleRecip (period x)
       amp = SigA.actualAmplitude x
       newAmp = DN.unrecip (SigA.actualSampleRate x) &*& amp
   in  SigA.Cons
          (Rate.Actual len)
          (Amp.Numeric newAmp)
          (SigC.Cons $ FourierG.transformBackward $ SigC.toPeriod $ SigA.body x)
{-
toFrequencySpectrum $ SigP.Cons (DN.frequency (4::Prelude.Double)) (SigA.Cons (DN.voltage (1::Prelude.Double)) (SigC.Cons [1, 0 Number.Complex.+: (1::Prelude.Double), -1, 0 Number.Complex.+: (-1)]))
toFrequencySpectrum $ SigP.Cons (DN.frequency (4::Prelude.Double)) (SigA.Cons (DN.voltage (1::Prelude.Double)) (SigC.Cons [0 Number.Complex.+: (1::Prelude.Double), -1, 0 Number.Complex.+: (-1), 1]))
toFrequencySpectrum $ SigP.Cons (DN.frequency (4::Prelude.Double)) (SigA.Cons (DN.voltage (1::Prelude.Double)) (SigC.Cons [1, -1,1, (-1) Number.Complex.+: (0::Prelude.Double)]))
-}


{- |
Fourier synthesis
-}
{-# INLINE fromFrequencySpectrum #-}
fromFrequencySpectrum ::
   (Trans.C q, Dim.C u, Dim.C v,
    SigG.Transform sig (Complex.T q)) =>
   SigA.T (Rate.Dimensional (Dim.Recip u) q) (Amp.Dimensional (Dim.Mul u v) q) (SigC.T (sig (Complex.T q))) ->
   SigA.T (Rate.Dimensional u q) (Amp.Dimensional v q) (SigC.T (sig (Complex.T q)))
fromFrequencySpectrum x =
   let len = period x
       amp = SigA.actualAmplitude x
       newAmp =
          DN.rewriteDimension
             (Dim.identityLeft .
              Dim.applyLeftMul Dim.cancelLeft . Dim.associateLeft)
             (DN.unrecip (SigA.actualSampleRate x) &*& amp)
   in  SigA.Cons
          (Rate.Actual len)
          (Amp.Numeric newAmp)
          (SigC.Cons $ FourierG.transformForward $ SigC.toPeriod $ SigA.body x)
