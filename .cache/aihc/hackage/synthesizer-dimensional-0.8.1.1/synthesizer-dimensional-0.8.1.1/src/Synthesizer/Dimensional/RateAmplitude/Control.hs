{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes


Control curves which can be used
as envelopes, for controlling filter parameters and so on.
-}
module Synthesizer.Dimensional.RateAmplitude.Control (
   {- * Primitives -}
   constant, constantVector,
   linear, line,
   exponential, exponential2, exponentialFromTo,
   cubicHermite,
   ) where

import qualified Synthesizer.Dimensional.Amplitude.Control as CtrlA
import qualified Synthesizer.State.Control as Ctrl

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc
import Synthesizer.Dimensional.Process
          (toTimeScalar, toGradientScalar, DimensionGradient, )
-- import Synthesizer.Dimensional.Process (($:), ($#), )
import Synthesizer.Dimensional.Signal.Private
          (toAmplitudeScalar, )

import qualified Synthesizer.State.Signal as Sig

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import qualified Algebra.Transcendental     as Trans
import qualified Algebra.RealField          as RealField
import qualified Algebra.Field              as Field
import qualified Algebra.RealRing           as RealRing
import qualified Algebra.Absolute           as Absolute

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()



{-# INLINE constant #-}
constant :: (Absolute.C y, Dim.C u, Dim.C v) =>
      DN.T v y {-^ value -}
   -> Proc.T s u t (SigA.R s v y y)
constant y = Proc.pure $ CtrlA.constant y

{- |
The amplitude must be positive!
This is not checked.
-}
{-# INLINE constantVector #-}
constantVector :: -- (Field.C y', Absolute.C y', Dim.C v) =>
      DN.T v y {-^ amplitude -}
   -> yv       {-^ value -}
   -> Proc.T s u t (SigA.R s v y yv)
constantVector y yv = Proc.pure $ CtrlA.constantVector y yv

{- Using the 'Ctrl.linear' instead of 'Ctrl.linearStable'
   the type class constraints would be weaker.
linear :: (Additive.C y, Field.C y', Absolute.C y', Dim.C v) =>
-}

{- |
Caution: This control curve can contain samples
with an absolute value greater than 1.

Linear curves starting with zero are impossible.
Maybe you prefer using 'line'.
-}
{-# INLINE linear #-}
linear ::
   (Field.C q, Absolute.C q, Dim.C u, Dim.C v) =>
      DN.T (DimensionGradient u v) q
               {-^ slope of the curve -}
   -> DN.T v q {-^ initial value -}
   -> Proc.T s u q (SigA.R s v q q)
linear slope y0 =
   let (amp,sgn) = DN.absSignum y0
   in  do steep <- toGradientScalar amp slope
          return (SigA.fromBody amp (Ctrl.linearMultiscale steep sgn))

{- |
Generates a finite ramp.
-}
{-# INLINE line #-}
line ::
   (RealField.C q, Dim.C u, Dim.C v) =>
      DN.T u q      {-^ duration of the ramp -}
   -> (DN.T v q, DN.T v q)
                    {-^ initial and final value -}
   -> Proc.T s u q (SigA.R s v q q)
line dur' (y0',y1') =
   (toTimeScalar dur') >>= \dur -> return $
      let amp = max (DN.abs y0') (DN.abs y1')
          y0  = toAmplitudeScalar z y0'
          y1  = toAmplitudeScalar z y1'
          z = SigA.fromBody amp
                 (Sig.take (floor dur)
                    (Ctrl.linearMultiscale ((y1-y0)/dur) y0))
      in  z

{-# INLINE exponential #-}
exponential :: (Trans.C q, Absolute.C q, Dim.C u, Dim.C v) =>
      DN.T u q {-^ time where the function reaches 1\/e of the initial value -}
   -> DN.T v q {-^ initial value -}
   -> Proc.T s u q (SigA.R s v q q)
exponential time y0 =
   (toTimeScalar time) >>= \t -> return $
      let (amp,sgn) = DN.absSignum y0
      in  SigA.fromBody amp (Ctrl.exponentialMultiscale t sgn)

{-
  take 1000 $ show (run (fixSampleRate 100 (exponential 0.1 1)) :: SigDouble)
-}

{-# INLINE exponential2 #-}
exponential2 :: (Trans.C q, Absolute.C q, Dim.C u, Dim.C v) =>
      DN.T u q {-^ half life, time where the function reaches 1\/2 of the initial value -}
   -> DN.T v q {-^ initial value -}
   -> Proc.T s u q (SigA.R s v q q)
exponential2 time y0 =
   (toTimeScalar time) >>= \t -> return $
      let (amp,sgn) = DN.absSignum y0
      in  SigA.fromBody amp (Ctrl.exponential2Multiscale t sgn)

{- |
Generate an exponential curve through two nodes.
-}
{-# INLINE exponentialFromTo #-}
exponentialFromTo ::
   (Trans.C q, RealRing.C q, Dim.C u, Dim.C v) =>
      DN.T u q      {-^ duration of the ramp -}
   -> (DN.T v q, DN.T v q)
                    {-^ initial and final value -}
   -> Proc.T s u q (SigA.R s v q q)
exponentialFromTo dur' (y0',y1') =
   (toTimeScalar dur') >>= \dur -> return $
      let amp = max (DN.abs y0') (DN.abs y1')
          y0  = toAmplitudeScalar z y0'
          y1  = toAmplitudeScalar z y1'
          z = SigA.fromBody amp
                 (Sig.take (floor dur)
                    (Ctrl.exponentialFromTo dur y0 y1))
      in  z



{-# INLINE cubicHermite #-}
cubicHermite ::
   (RealField.C q, Dim.C u, Dim.C v) =>
      (DN.T u q, (DN.T v q, DN.T (DimensionGradient u v) q))
   -> (DN.T u q, (DN.T v q, DN.T (DimensionGradient u v) q))
   -> Proc.T s u q (SigA.R s v q q)
cubicHermite (t0', (y0',dy0')) (t1', (y1',dy1')) =
   let amp = max (DN.abs y0') (DN.abs y1')
   in  do t0  <- toTimeScalar t0'
          t1  <- toTimeScalar t1'
          dy0 <- toGradientScalar amp dy0'
          dy1 <- toGradientScalar amp dy1'
          return $
             let y0 = toAmplitudeScalar z y0'
                 y1 = toAmplitudeScalar z y1'
                 z = SigA.fromBody amp (Ctrl.cubicHermite (t0, (y0,dy0)) (t1, (y1,dy1)))
              in z
