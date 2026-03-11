{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Chebyshev lowpass and highpass
-}
module Synthesizer.Plain.Filter.Recursive.Chebyshev (
   ParameterA, parameterA, partialParameterA,
   ParameterB, parameterB, partialParameterB,
   canonicalizeParameterA,
   causalA, runAPole, causalAPole,
   causalB, runBPole, causalBPole,
   lowpassACausalPole, highpassACausalPole,
   lowpassBCausalPole, highpassBCausalPole,
   lowpassAPole, highpassAPole,
   lowpassBPole, highpassBPole,
   -- used in LLVM.Filter.Chebyshev
   makeCirclePoints,
   ) where

import Synthesizer.Plain.Filter.Recursive (Passband(Lowpass,Highpass), Pole(Pole, poleResonance))
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrderCascade as Cascade
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as Filt2
import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Causal.Process as Causal
import Control.Arrow ((>>>), (^>>), (&&&), )

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Field                 as Field
import qualified Algebra.Ring                  as Ring

import Number.Complex (real, imag, cis, )
import qualified Number.Complex as Complex

import qualified Data.StorableVector as SV
import Foreign.Storable (Storable)

import NumericPrelude.Numeric
import NumericPrelude.Base




circleList, circleListSlow, _circleListFast :: (Trans.C a) => a -> [Complex.T a]
circleList = circleListSlow

circleListSlow x =
   map cis $ map (x*) $ iterate (2+) 1

_circleListFast x =
   let z1 = cis x
       z2 = z1^2
   in  iterate (z2*) z1


makeCirclePoints :: (Trans.C a) => Int -> [Complex.T a]
makeCirclePoints order =
   take order (circleList (pi / (4 * fromIntegral order)))

-- | compute the partial filter of the second order from the pole information
{-
It's worth to think it over whether this routine could be used for the Butterworth filter.
But whereas this function is specialized to the zeros of the denominator polynomial
for the Butterworth filter the quadratic factors of the polynomial can be determined
more efficiently than the zeros.
-}
partialLowpassParameterA, partialLowpassParameterB :: (Trans.C a) =>
   Int -> a -> a -> Complex.T a -> Filt2.Parameter a
{-
partialLowpassParameterA order ratio freq =
   let {- if ratio == (sqrt 2) then the product of the normalization factors is
          2^(1-2*order) -}
--       bn = asinh (ratio/sqrt(1-ratio^2)) / fromIntegral (2*order)
       bn = (log(1+ratio) - log(1-ratio^2)/2) / fromIntegral (2*order)
       coshbn = cosh bn
       sinhbn = sinh bn

       phi     = pi*freq
       sinphi  = sin phi
       cosphi  = cos phi
       sinphi2 = sinphi^2

   in  \c ->
          let re      =   real c * coshbn; re2 = re^2
              im      = - imag c * sinhbn; im2 = im^2
              cpims   = cosphi + im*sinphi
              cmims   = cosphi - im*sinphi
              resin2  = re2*sinphi2
              denom   = - cmims^2 - resin2
              vol     = sqrt ((1-re2-im2)^2 + 4*im2)
              c0      = vol * sinphi2 / denom
          in  Filt2.Parameter
                 c0 (2*c0) c0
                 (-2*(cpims*cmims - resin2)/denom) ((cpims^2 + resin2)/denom)
-}

partialLowpassParameterA order ratio freq =
   let {- if ratio == (sqrt 2) then the product of the normalization factors is
          2^(1-2*order) -}
--       bn = asinh (ratio/sqrt(1-ratio^2)) / fromIntegral (2*order)
       bn = (log(1+ratio) - log(1-ratio^2)/2) / fromIntegral (2*order)
       coshbn = cosh bn
       sinhbn = sinh bn
--       cosh2bn = (cosh(2*bn)-1)/2 = sinhbn2
       coshbn2 = coshbn^2
       sinhbn2 = sinhbn^2

       phi     = pi*freq
       sinphi  = sin phi
       cosphi  = cos phi
       sinphi2 = sinphi^2

       sinhbnsinphi   = sinhbn*sinphi
--       sinhbn2sinphi2 = sinhbn2*sinphi2
       coshbn2sinphi2 = coshbn2*sinphi2

   in  \c ->
          let re      = real c
              im      = imag c
              imss    = im * sinhbnsinphi
              cpims   = cosphi - imss
              cmims   = cosphi + imss
              resin2  = re^2 * coshbn2sinphi2
              denom   = - cmims^2 - resin2
              c0      = (im^2 + sinhbn2)*sinphi2 / denom
          in  Filt2.Parameter
                 c0 (2*c0) c0
                 (-2*(cpims*cmims - resin2)/denom) ((cpims^2 + resin2)/denom)

{-
partialLowpassParameterA order ratio freq =
   let {- if ratio == (sqrt 2) then the product of the normalization factors is
          2^(1-2*order) -}
       bn = asinh (ratio/sqrt(1-ratio^2)) / fromIntegral (2*order)
       sinhbnd = 2 * sinh bn
       cosh2bn = (cosh(2*bn)-1)/2

       phi       = pi*freq
       sinphi    = sin phi
       cosphi    = cos phi
       sinphi2   = sinphi^2
--       cosphi2   = cosphi^2
       sincosphi = sinphi*cosphi

   in  \c ->
          let imd     = - imag c * sinhbnd
              re2pim2 = cosh2bn + real c ^ 2
              ri2sp2  = (re2pim2-1)*sinphi2
              cpims2  = 1 + ri2sp2 + imd*sincosphi
              cmims2  = 1 + ri2sp2 - imd*sincosphi
              cpmims  = 1 - (re2pim2+1)*sinphi2
              denom   = - cmims2
              vol     = sqrt (ri2sp2^2 + (imd*sinphi2)^2)
              c0      = vol / denom
          in  Filt2.Parameter
                 c0 (2*c0) c0
                 (-2*cpmims/denom) (cpims2/denom)
-}

{-
partialLowpassParameterA order ratio freq =
   let {- if ratio == (sqrt 2) then the product of the normalization factors is
          2^(1-2*order) -}
       bn = asinh (ratio/sqrt(1-ratio^2)) / fromIntegral (2*order)
       coshbn = cosh bn
       sinhbn = sinh bn

       phi       = pi*freq
       sinphi    = sin phi
       cosphi    = cos phi
       sinphi2   = sinphi^2
--       cosphi2   = cosphi^2
       sincosphi = sinphi*cosphi

   in  \c ->
          let re      =   real c * coshbn; re2 = re^2
              im      = - imag c * sinhbn; im2 = im^2
              re2pim2 = re2+im2
              cpims2  = 1 + (re2pim2-1)*sinphi2 + 2*im*sincosphi
              cmims2  = 1 + (re2pim2-1)*sinphi2 - 2*im*sincosphi
              cpmims  = 1 - (re2pim2+1)*sinphi2
              denom   = - cmims2
              vol     = sqrt ((re2pim2-1)^2 + 4*im2)
              c0      = vol * sinphi2 / denom
          in  Filt2.Parameter
                 c0 (2*c0) c0
                 (-2*cpmims/denom) (cpims2/denom)
-}

{-
partialLowpassParameterB order ratio freq =
   let -- bn = asinh (sqrt(1-ratio^2)/ratio) / fromIntegral (2*order)
       bn = (log(1+sqrt(1-ratio^2)) - log ratio) / fromIntegral (2*order)
       coshbn  = cosh bn
       sinhbn  = sinh bn
       coshbn2 = coshbn^2

       phi     = pi*freq
       sinphi  = sin phi
       cosphi  = cos phi
       sinphi2 = sinphi^2
       cosphi2 = cosphi^2

   in  \c ->
          let re      =   real c * coshbn
              im      = - imag c * sinhbn
              spimc   = sinphi + im*cosphi
              smimc   = sinphi - im*cosphi
              recos2  = re^2 * cosphi2
              denom   = smimc^2 + recos2
              a02cosphi2 = real c ^ 2 * cosphi2
              c0      = (sinphi2 + a02cosphi2) / denom
              c1      = (sinphi2 - a02cosphi2) / denom
          in  Filt2.Parameter
                 c0 (2*c1) c0
                 (-2*(spimc*smimc - recos2)/denom) (-(spimc^2 + recos2)/denom)
-}

partialLowpassParameterB order ratio freq =
   let -- bn = asinh (sqrt(1-ratio^2)/ratio) / fromIntegral (2*order)
       bn = (log(1+sqrt(1-ratio^2)) - log ratio) / fromIntegral (2*order)
       coshbn  = cosh bn
       sinhbn  = sinh bn
       coshbn2 = coshbn^2

       phi     = pi*freq
       sinphi  = sin phi
       cosphi  = cos phi
       sinphi2 = sinphi^2
       cosphi2 = cosphi^2

       sinhbncosphi = sinhbn*cosphi

   in  \c ->
          let a02cosphi2 = real c ^ 2 * cosphi2
              imsc    = imag c * sinhbncosphi
              spimc   = sinphi - imsc
              smimc   = sinphi + imsc
              recos2  = a02cosphi2 * coshbn2
              denom   = smimc^2 + recos2
              c0      = (sinphi2 + a02cosphi2) / denom
              c1      = (sinphi2 - a02cosphi2) / denom
          in  Filt2.Parameter
                 c0 (2*c1) c0
                 (-2*(spimc*smimc - recos2)/denom) (-(spimc^2 + recos2)/denom)


-- * use second order filter parameters for control

{-# INLINE partialParameter #-}
partialParameter ::
   (Field.C a) =>
   (a -> a -> Complex.T a -> Filt2.Parameter a) ->
   Passband -> a -> Complex.T a -> a -> Filt2.Parameter a
partialParameter lowpassParameter kind ratio c freq =
   Filt2.adjustPassband kind
      (flip (lowpassParameter ratio) c)
      freq

{-# INLINE partialParameterA #-}
{-# INLINE partialParameterB #-}
partialParameterA, partialParameterB ::
   (Trans.C a) =>
   Passband -> Int -> a -> Complex.T a -> a -> Filt2.Parameter a
partialParameterA kind order =
   partialParameter (partialLowpassParameterA order) kind
partialParameterB kind order =
   partialParameter (partialLowpassParameterB order) kind

{-
We could prevent definition of an extra parameter type
by applying application to one of the filters using Filt2.amplify.
-}
type ParameterA a = (a, Cascade.Parameter a)

{-# INLINE parameterA #-}
parameterA ::
   (Trans.C a, Storable a) =>
   Passband -> Int -> Pole a -> ParameterA a
parameterA kind order =
   -- I hope that the 'let' is floated out of a 'map'
   let circleVec = SV.pack (makeCirclePoints order)
   in  \ (Pole ratio freq) ->
          (ratio,
           Cascade.Parameter $
           SV.map (\c -> partialParameterA kind order ratio c freq) $
           circleVec)

{-# INLINE canonicalizeParameterA #-}
canonicalizeParameterA ::
   (Ring.C a, Storable a) =>
   ParameterA a -> Cascade.Parameter a
canonicalizeParameterA (amp, Cascade.Parameter p) =
   Cascade.Parameter
      (SV.switchL SV.empty (\h -> SV.cons (Filt2.amplify amp h)) p)


type ParameterB a = Cascade.Parameter a

{-# INLINE parameterB #-}
parameterB ::
   (Trans.C a, Storable a) =>
   Passband -> Int -> Pole a -> ParameterB a
parameterB kind order =
   -- I hope that the 'let' is floated out of a 'map'
   let circleVec = SV.pack (makeCirclePoints order)
   in  \ (Pole ratio freq) ->
           Cascade.Parameter $
           SV.map (\c -> partialParameterB kind order ratio c freq) $
           circleVec

{-
{-# INLINE modifierB #-}
modifierB ::
   (Ring.C a, Module.C a v, Storable a, Storable v) =>
   Int ->
   Modifier.Simple (Cascade.State v) (Cascade.Parameter a) v v
modifierB =
   Cascade.modifierB
-}

{-# INLINE causalA #-}
causalA :: (Ring.C a, Module.C a v, Storable a, Storable v) =>
   Int ->
   Causal.T (ParameterA a, v) v
causalA order =
   Causal.map (snd.fst) &&& Causal.map (\((ratio,_), y) -> ratio *> y)
    >>> Cascade.causal order

{-# INLINE causalB #-}
causalB :: (Ring.C a, Module.C a v, Storable a, Storable v) =>
   Int ->
   Causal.T (ParameterB a, v) v
causalB =
   Cascade.causal




-- * directly use frequency as control parameter

runAPole, runBPole :: (Trans.C a, Module.C a v) =>
   Passband -> Int -> Sig.T a -> Sig.T a -> Sig.T v -> Sig.T v
runAPole kind order ratios freqs =
   let makePartialFilter c =
          Filt2.run
             (zipWith
                 (\ratio freq -> partialParameterA kind order ratio c freq)
                 ratios freqs)
   in  foldl (.) (zipWith (*>) ratios)
          (map makePartialFilter (makeCirclePoints order))

runBPole kind order ratios freqs =
   let makePartialFilter c =
          Filt2.run
             (zipWith
                 (\ratio freq -> partialParameterB kind order ratio c freq)
                 ratios freqs)
   in  foldl (.) id (map makePartialFilter (makeCirclePoints order))


causalAPole, causalBPole :: (Trans.C a, Module.C a v) =>
   Passband -> Int -> Causal.T (Pole a, v) v
causalAPole kind order =
   let {-# INLINE makePartialFilter #-}
       makePartialFilter c =
          Causal.first (Causal.map (\(Pole ratio freq) ->
             partialParameterA kind order ratio c freq)) >>>
          Filt2.causal
   in  (\(p, y) -> (p, poleResonance p *> y)) ^>>
       (Causal.chainControlled $
        map makePartialFilter $
        makeCirclePoints order)

causalBPole kind order =
   let {-# INLINE makePartialFilter #-}
       makePartialFilter c =
          Causal.first (Causal.map (\(Pole ratio freq) ->
             partialParameterB kind order ratio c freq)) >>>
          Filt2.causal
   in  Causal.chainControlled $
       map makePartialFilter $
       makeCirclePoints order


lowpassACausalPole, highpassACausalPole,
 lowpassBCausalPole, highpassBCausalPole ::
   (Trans.C a, Module.C a v) =>
   Int -> Causal.T (Pole a, v) v
lowpassACausalPole  = causalAPole Lowpass
highpassACausalPole = causalAPole Highpass

lowpassBCausalPole  = causalBPole Lowpass
highpassBCausalPole = causalBPole Highpass


lowpassAPole, highpassAPole, lowpassBPole, highpassBPole ::
   (Trans.C a, Module.C a v) =>
   Int -> Sig.T a -> Sig.T a -> Sig.T v -> Sig.T v
lowpassAPole  = runAPole Lowpass
highpassAPole = runAPole Highpass

lowpassBPole  = runBPole Lowpass
highpassBPole = runBPole Highpass
