{-# LANGUAGE NoImplicitPrelude #-}
{- |
Tone generators with measures for band-limitation.

They are not exactly band-limiting because this would cause infinite lag.
Instead we use only cubic interpolation polynomials.
This still incurs a small lag.

<https://youtu.be/lpM4Tawq-XU>
-}
module Synthesizer.Plain.Oscillator.BandLimited where

import qualified Synthesizer.Plain.Signal as Sig

import qualified Algebra.RealField as RealField

import NumericPrelude.Numeric
import NumericPrelude.Base



{-
sinc approximation, that could be used for band-limited oscillators:

GP.plotFuncs [] (GP.linearScale 1000 (-2,2::Double)) [\x -> if x<0 then (if x< -1 then (x+1)*(x+2)*(x+2) else 1-x*x*2-x*x*x) else (if x<1 then 1-x*x*2+x*x*x else -(x-1)*(x-2)*(x-2)), \x -> if x==0 then 1 else sin (pi*x)/(pi*x)]

Has the same tangent as sinc-pi at point 1.

Cf.
DSP.Filter.FIR.PolyInterp
Integral Sine: gsl_sf_Si
-}



{- | impulse train with static frequency -}
staticImpulses :: (RealField.C a) => a -> a -> Sig.T a
staticImpulses phase = freqModImpulses phase . repeat

{- | impulse train with modulated frequency -}
freqModImpulses :: (RealField.C a) => a -> Sig.T a -> Sig.T a
freqModImpulses phase =
   (\ ~(~(_,remaining),xs) -> xs ++ remaining) .
   Sig.mapAccumL
      (\freq (p0,xs0) ->
         let p1 = p0+freq
             (p2, xs1) =
               if p1>=1
               then
                  let p1frac=fraction p1
                      t=p1frac/freq
                      t_2  = t*t;         y0 =  t_2*(t-1)
                      t1_2 = (t-1)*(t-1); y3 = -t1_2*t
                  in (p1frac, xs0 + [y0, 1-t1_2+y3, 1-t_2+y0, y3])
               else (p1, xs0)
             (x3,xs3) =
               case xs1 of
                  [] -> (0,[])
                  x2:xs2 -> (x2,xs2)
         in Just $ (x3, (p2,xs3)))
      (phase,[])
