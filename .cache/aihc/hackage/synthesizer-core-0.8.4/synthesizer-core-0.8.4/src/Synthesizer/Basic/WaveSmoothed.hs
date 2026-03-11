{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2006
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Waveforms which are smoothed according to the oscillator frequency
in order to suppress aliasing effects.
-}
module Synthesizer.Basic.WaveSmoothed (
   T,
   fromFunction,
   fromWave,
   fromControlledWave,

   raise,
   amplify,
   distort,
   apply,

   sine,
   cosine,
   saw,
   square,
   triangle,

   Wave.Harmonic,
   Wave.harmonic,
   composedHarmonics,
   ) where


import qualified Synthesizer.Basic.Wave  as Wave
import qualified Synthesizer.Basic.Phase as Phase

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Module                as Module
import qualified Algebra.Field                 as Field
import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import qualified MathObj.Polynomial as Poly
import qualified Number.Complex     as Complex

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Definition and construction -}

newtype T t y = Cons {decons :: t -> Phase.T t -> y}


{-# INLINE fromFunction #-}
fromFunction :: (t -> t -> y) -> (T t y)
fromFunction wave =
   Cons (\f p -> wave f (Phase.toRepresentative p))

{- |
Use this function for waves which are sufficiently smooth.
If the Nyquist frequency is exceeded the wave is simply replaced
by a constant zero wave.
-}
{-# INLINE fromWave #-}
fromWave ::
   (Field.C t, RealRing.C t, Additive.C y) =>
   Wave.T t y -> (T t y)
fromWave wave =
   fromControlledWaveAux (\f -> if abs f >= 1/2 then zero else wave)

{-# INLINE fromControlledWave #-}
fromControlledWave ::
   (Field.C t, RealRing.C t, Additive.C y) =>
   (t -> Wave.T t y) -> (T t y)
fromControlledWave wave =
   fromControlledWaveAux (\f0 ->
      let f = abs f0
      in  if f >= 1/2
            then zero
            else wave f)

{-# INLINE fromControlledWaveAux #-}
fromControlledWaveAux :: (t -> Wave.T t y) -> (T t y)
fromControlledWaveAux wave =
   Cons (\f p -> Wave.apply (wave f) p)


{- * Operations on waves -}

{-# INLINE raise #-}
raise :: (Additive.C y) => y -> T t y -> T t y
raise y = distort (y+)

{-# INLINE amplify #-}
amplify :: (Ring.C y) => y -> T t y -> T t y
amplify k = distort (k*)

{-# INLINE distort #-}
distort :: (y -> z) -> T t y -> T t z
distort g (Cons w) = Cons (\f p -> g (w f p))

{-# INLINE apply #-}
apply :: T t y -> (t -> Phase.T t -> y)
apply = decons



instance Additive.C y => Additive.C (T t y) where
   {-# INLINE zero #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   {-# INLINE negate #-}
   zero = Cons (const zero)
   (+) (Cons w) (Cons v) = Cons (\f p -> w f p + v f p)
   (-) (Cons w) (Cons v) = Cons (\f p -> w f p - v f p)
   negate = distort negate


instance Module.C a y => Module.C a (T t y) where
   {-# INLINE (*>) #-}
   s *> w = distort (s*>) w




{- * Examples -}

{- ** unparameterized -}

{- | map a phase to value of a sine wave -}
{-# INLINE sine #-}
sine :: (Trans.C a, RealRing.C a) => T a a
sine = fromWave Wave.sine

{-# INLINE cosine #-}
cosine :: (Trans.C a, RealRing.C a) => T a a
cosine = fromWave Wave.cosine


{- | saw tooth,
it's a ramp down in order to have a positive coefficient for the first partial sine
-}
{-# INLINE saw #-}
saw :: (RealRing.C a, Field.C a) => T a a
saw =
   fromControlledWave (\f -> Wave.triangleAsymmetric (2*f-1))


{- | square -}
{-# INLINE square #-}
square :: (RealRing.C a, Field.C a) => T a a
square =
   fromControlledWave (\f -> Wave.trapezoid (1-2*f))


{- | triangle -}
{-# INLINE triangle #-}
triangle :: (RealRing.C a, Field.C a) => T a a
triangle = fromWave Wave.triangle



{- |
Specify the wave by its harmonics.

The function is implemented quite efficiently
by applying the Horner scheme to a polynomial with complex coefficients
(the harmonic parameters)
using a complex exponential as argument.
-}
{-# INLINE composedHarmonics #-}
composedHarmonics :: (Trans.C a, RealRing.C a) => [Wave.Harmonic a] -> T a a
composedHarmonics hs =
   let c = map (\h -> Complex.fromPolar (Wave.harmonicAmplitude h)
                   (2*pi * Phase.toRepresentative (Wave.harmonicPhase h))) hs
       -- @take (ceiling (1/(2*f)))@ would fail for small @f@ especially @f==zero@
       trunc f =
          map snd . takeWhile ((<1/2) . fst) . zip (iterate (abs f +) zero)
   in  fromControlledWaveAux $ \f ->
          Wave.distort
             (Complex.imag . Poly.evaluate (Poly.fromCoeffs (trunc f c)))
             Wave.helix
{-
GNUPlot.plotFunc [] (GNUPlot.linearScale 1000 (0,1::Double)) (composedHarmonics [harmonic 0 0, harmonic 0 0, harmonic 0 0, harmonic 0.25 1])
-}
