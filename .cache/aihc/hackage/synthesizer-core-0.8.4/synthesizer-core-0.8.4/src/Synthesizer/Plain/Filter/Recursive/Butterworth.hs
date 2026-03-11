{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Butterworth lowpass and highpass
-}
module Synthesizer.Plain.Filter.Recursive.Butterworth (
   Parameter,
   causal,
   causalPole,
   highpassCausalPole, highpassPole,
   lowpassCausalPole,  lowpassPole,
   modifier,
   parameter,
   partialParameter,
   runPole,

   -- used in Dimensional.Causal.FilterParameter
   checkedHalf,
   -- used in LLVM.Filter.Butterworth
   partialRatio,
   makeSines,
   ) where

import Synthesizer.Plain.Filter.Recursive (Passband(Lowpass,Highpass), Pole(Pole))
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrderCascade as Cascade
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as Filt2
import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Synthesizer.Causal.Process as Causal
import Control.Arrow ((>>>), )

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Ring                  as Ring

import qualified Data.StorableVector as SV
import Foreign.Storable (Storable)

import NumericPrelude.Numeric
import NumericPrelude.Base



sineList, _sineListSlow, sineListFast :: (Trans.C a) => a -> [a]
sineList = sineListFast

_sineListSlow x =
   map sin $ map (x*) $ iterate (2+) 1

sineListFast x =
   let sinx  = sin x
       cos2x = 2 - 4*sinx*sinx
       --cos2x = 2 * cos (2*x)
       sines = (-sinx) : sinx :
                  zipWith (\y1 y0 -> cos2x * y0 - y1) sines (tail sines)
   in  tail sines

makeSines :: (Trans.C a) =>
   Int -> [a]
makeSines order =
   take (checkedHalf "makeSines" order) (sineList (pi / fromIntegral (2*order)))

partialRatio :: (Trans.C a) =>
   Int -> a -> a
partialRatio order ratio =
   (1/ratio^2-1) ** (-1 / fromIntegral (2*order))



_partialLowpassParameterInstable, partialLowpassParameter :: (Trans.C a) =>
   a -> a -> a -> Filt2.Parameter a

{- must handle infinite values when 'freq' approaches 0.5 -}
_partialLowpassParameterInstable ratio freq sinw =
   let wc    = ratio * tan (pi*freq)
       sinw2 = 2 * wc * sinw
       wc2   = wc * wc
       denom = wc2 + sinw2 + 1
       c0    = wc2 / denom
   in  Filt2.Parameter c0 (2*c0) c0
          (2*(1-wc2)/denom) ((-wc2+sinw2-1)/denom)

-- using ratio disallows simplification by trigonometric Pythagoras' theorem
partialLowpassParameter ratio freq =
   let phi      = pi*freq
       rsin2phi = ratio * sin (2*phi)
       cosphi2  = cos phi ^ 2
       c0d      = (ratio * sin phi) ^ 2
       d1d      = (cosphi2 - c0d) * 2
       ratsin   = cosphi2 + c0d
   in  \sinw ->
          let rsinsin = rsin2phi * sinw
              denom   = rsinsin + ratsin
              d2d     = rsinsin - ratsin
              c0      = c0d / denom
              d1      = d1d / denom
              d2      = d2d / denom
          in  Filt2.Parameter c0 (2*c0) c0 d1 d2


-- * use second order filter parameters for control

type Parameter a = Cascade.Parameter a

{-# INLINE parameter #-}
parameter ::
   (Trans.C a, Storable a) =>
   Passband -> Int -> Pole a -> Parameter a
parameter kind order =
   -- I hope that the 'let' is floated out of a 'map'
   let sinesVec = SV.pack (makeSines order)
   in  \ (Pole ratio freq) ->
           Cascade.Parameter $
           SV.map
              (\sinw ->
                 partialParameter kind (partialRatio order ratio) sinw freq) $
           sinesVec

partialParameter ::
   Trans.C a =>
   Passband -> a -> a -> a -> Filt2.Parameter a
partialParameter kind partRatio sinw freq =
   Filt2.adjustPassband kind
      (flip (partialLowpassParameter partRatio) sinw)
      freq

{-# INLINE modifier #-}
modifier ::
   (Ring.C a, Module.C a v, Storable a, Storable v) =>
   Int ->
   Modifier.Simple (Cascade.State v) (Parameter a) v v
modifier =
   Cascade.modifier

{-# INLINE causal #-}
causal :: (Ring.C a, Module.C a v, Storable a, Storable v) =>
   Int ->
   Causal.T (Parameter a, v) v
causal order =
   Cascade.causal (checkedHalf "causal" order)


{-# INLINE checkedHalf #-}
checkedHalf :: String -> Int -> Int
checkedHalf funcName order =
   let (order2,r) = divMod order 2
   in  if r==0
         then order2
         else error $ "Butterworth."++funcName++": order must be even"

{-
lowpassCausal, highpassCausal :: (Trans.C a, Module.C a v) =>
   Int -> Causal.T (Parameter a, v) v
lowpassCausal  = causal Lowpass
highpassCausal = causal Highpass

lowpass, highpass :: (Trans.C a, Module.C a v) =>
   Int -> Sig.T (Parameter a) -> Sig.T v -> Sig.T v
lowpass  = run Lowpass
highpass = run Highpass
-}


-- * directly use frequency as control parameter

{- |
When called as @runPole kind order ratio freqs@,
the filter amplifies frequency 0 with factor 1
and frequency @freq@ with factor @ratio@.

It uses the frequency and ratio information directly
and thus cannot benefit from efficient parameter interpolation
(asynchronous run of a ControlledProcess).
-}
runPole :: (Trans.C a, Module.C a v) =>
   Passband -> Int -> Sig.T a -> Sig.T a -> Sig.T v -> Sig.T v
runPole kind order ratios freqs =
   let makePartialFilter s =
          Filt2.run $
          zipWith
             (\ratio freq ->
                partialParameter kind (partialRatio order ratio) s freq)
             ratios freqs
   in  foldl (.) id (map makePartialFilter (makeSines order))

causalPole :: (Trans.C a, Module.C a v) =>
   Passband -> Int -> Causal.T (Pole a, v) v
causalPole kind order =
   let {-# INLINE makePartialFilter #-}
       makePartialFilter s =
          Causal.first
             (Causal.map (\(Pole ratio freq) ->
                partialParameter kind (partialRatio order ratio) s freq))
          >>>
          Filt2.causal
   in  Causal.chainControlled $ map makePartialFilter $ makeSines order


lowpassCausalPole, highpassCausalPole :: (Trans.C a, Module.C a v) =>
   Int -> Causal.T (Pole a, v) v
lowpassCausalPole  = causalPole Lowpass
highpassCausalPole = causalPole Highpass

lowpassPole, highpassPole :: (Trans.C a, Module.C a v) =>
   Int -> Sig.T a -> Sig.T a -> Sig.T v -> Sig.T v
lowpassPole  = runPole Lowpass
highpassPole = runPole Highpass
