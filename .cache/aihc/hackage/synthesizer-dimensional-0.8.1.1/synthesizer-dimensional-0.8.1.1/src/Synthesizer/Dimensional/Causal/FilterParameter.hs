{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2011
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Causal.FilterParameter (
   -- * Recursive

   -- ** Without resonance
   highpassFromFirstOrder,
   lowpassFromFirstOrder,
   firstOrder, FirstOrderGlobal,

   butterworthLowpass,
   butterworthHighpass,
   chebyshevALowpass,
   chebyshevAHighpass,
   chebyshevBLowpass,
   chebyshevBHighpass,
   SecondOrderCascadeGlobal,

   -- ** Allpass
   allpassCascade, AllpassCascadeGlobal,
   allpassPhaser, AllpassPhaserGlobal,
   FiltR.allpassFlangerPhase,

   -- ** With resonance
   universal, UniversalGlobal,
   highpassFromUniversal,
   bandpassFromUniversal,
   lowpassFromUniversal,
   bandlimitFromUniversal,

   moogLowpass, MoogLowpassGlobal,
) where

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Amplitude as Amp
-- import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Causal.ControlledProcess as CCProc
import qualified Synthesizer.Dimensional.Causal.Process as CausalD
import qualified Synthesizer.Dimensional.Arrow as ArrowD
import qualified Synthesizer.Causal.Process as Causal
import Control.Arrow (Arrow, arr, (<<^), (^<<), )

-- import Synthesizer.Dimensional.Process ((.:), (.^), )

import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat

import Synthesizer.Dimensional.Process
   (toFrequencyScalar, )

import qualified Synthesizer.Dimensional.Rate.Filter as FiltR

-- import qualified Synthesizer.Interpolation as Interpolation
-- import qualified Synthesizer.State.Filter.Delay as Delay
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder  as Filt1
import qualified Synthesizer.Plain.Filter.Recursive.Allpass     as Allpass
import qualified Synthesizer.Plain.Filter.Recursive.Universal   as UniFilter
import qualified Synthesizer.Plain.Filter.Recursive.Moog        as Moog
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrderCascade as Cascade
import qualified Synthesizer.Plain.Filter.Recursive.Butterworth as Butter
import qualified Synthesizer.Plain.Filter.Recursive.Chebyshev   as Cheby
import qualified Synthesizer.Plain.Filter.Recursive    as FiltRec

import Synthesizer.Utility (affineComb, )

import qualified Algebra.DimensionTerm       as Dim

import qualified Number.NonNegative     as NonNeg

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Field          as Field
import qualified Algebra.Module         as Module

import Foreign.Storable (Storable)

-- import Control.Monad(liftM2)

import Data.Tuple.HT (swap, mapFst, )

import NumericPrelude.Numeric hiding (negate)
import NumericPrelude.Base as P
import Prelude ()



{-# INLINE highpassFromFirstOrder #-}
{-# INLINE lowpassFromFirstOrder #-}
highpassFromFirstOrder, lowpassFromFirstOrder ::
   CausalD.Single s amp amp (Filt1.Result yv) yv
highpassFromFirstOrder  = homogeneousMap Filt1.highpass_
lowpassFromFirstOrder   = homogeneousMap Filt1.lowpass_


data FirstOrderGlobal = FirstOrderGlobal

{-# INLINE firstOrder #-}
firstOrder ::
   (Dim.C u, Trans.C q, Arrow arrow) =>
   Proc.T s u q
      (ArrowD.T arrow
         (Sample.Dimensional (Dim.Recip u) q q)
         (Sample.T FirstOrderGlobal (CCProc.RateDep s (Filt1.Parameter q))))
firstOrder =
   flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
      ArrowD.Cons $ \ (Amp.Numeric freqAmp) ->
         swap $
         (FirstOrderGlobal,
          arr $
          \ freq ->
              (CCProc.RateDep $
               Filt1.parameter $
               freq * toFreq freqAmp))

instance Amp.C FirstOrderGlobal where
instance Amp.Primitive FirstOrderGlobal where primitive = FirstOrderGlobal

instance (Module.C q yv) =>
   CCProc.C FirstOrderGlobal (Filt1.Parameter q)
      (Sample.T amp yv) (Sample.T amp (Filt1.Result yv)) where
   process =
      return $ CausalD.consFlip $ \ (FirstOrderGlobal, amp) ->
         (amp, Filt1.causal <<^ mapFst CCProc.unRateDep)



{-# INLINE butterworthLowpass #-}
{-# INLINE butterworthHighpass #-}
{-# INLINE chebyshevALowpass #-}
{-# INLINE chebyshevAHighpass #-}
{-# INLINE chebyshevBLowpass #-}
{-# INLINE chebyshevBHighpass #-}

type SecondOrderCascade s u q arrow =
   Proc.T s u q
      (ArrowD.T arrow
         (Sample.Dimensional Dim.Scalar q q,
          -- Sample.Flat q,
          Sample.Dimensional (Dim.Recip u) q q)
         (Sample.T SecondOrderCascadeGlobal
             (CCProc.RateDep s (Cascade.Parameter q))))


newtype SecondOrderCascadeGlobal = SecondOrderCascadeGlobal Int


butterworthLowpass, butterworthHighpass ::
   (Arrow arrow, Trans.C q, Storable q, Dim.C u) =>
   NonNeg.Int   {- ^ Order of the filter, must be even,
                     the higher the order, the sharper is the separation of frequencies. -}  ->
   SecondOrderCascade s u q arrow


chebyshevALowpass, chebyshevAHighpass ::
   (Arrow arrow, Trans.C q, Storable q, Dim.C u) =>
   NonNeg.Int ->
   SecondOrderCascade s u q arrow


chebyshevBLowpass, chebyshevBHighpass ::
   (Arrow arrow, Trans.C q, Storable q, Dim.C u) =>
   NonNeg.Int ->
   SecondOrderCascade s u q arrow

butterworthLowpass  = higherOrderNoReso (Butter.checkedHalf "Parameter.butterworthLowpass") (Butter.parameter FiltRec.Lowpass)
butterworthHighpass = higherOrderNoReso (Butter.checkedHalf "Parameter.butterworthHighpass") (Butter.parameter FiltRec.Highpass)
chebyshevALowpass   = higherOrderNoReso id (\n -> Cheby.canonicalizeParameterA . Cheby.parameterA FiltRec.Lowpass n)
chebyshevAHighpass  = higherOrderNoReso id (\n -> Cheby.canonicalizeParameterA . Cheby.parameterA FiltRec.Highpass n)
chebyshevBLowpass   = higherOrderNoReso id (Cheby.parameterB FiltRec.Lowpass)
chebyshevBHighpass  = higherOrderNoReso id (Cheby.parameterB FiltRec.Highpass)

{-# INLINE higherOrderNoReso #-}
higherOrderNoReso ::
   (Arrow arrow, Field.C a, Storable a, Dim.C u) =>
   (Int -> Int) ->
   (Int -> FiltRec.Pole a -> Cascade.Parameter a) ->
   NonNeg.Int ->
   SecondOrderCascade s u a arrow

higherOrderNoReso adjustOrder mkParam order =
   let orderInt = NonNeg.toNumber order
   in  flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
          ArrowD.Cons $ \ (resoAmp, Amp.Numeric freqAmp) ->
             swap $
             (SecondOrderCascadeGlobal $ adjustOrder orderInt,
              let k = toFreq freqAmp
              in  arr $
                  \ (reso, freq) ->
                      CCProc.RateDep $
                      mkParam orderInt $
                      FiltRec.Pole (Flat.amplifySample resoAmp reso) (k*freq))


instance Amp.C SecondOrderCascadeGlobal where

instance (Storable q, Storable yv, Module.C q yv) =>
   CCProc.C SecondOrderCascadeGlobal (Cascade.Parameter q)
      (Sample.T amp yv) (Sample.T amp yv) where
   process =
      return $ CausalD.consFlip $ \ (SecondOrderCascadeGlobal orderInt, amp) ->
         (amp, Cascade.causal orderInt <<^ mapFst CCProc.unRateDep)


{-
{-# INLINE butterworthLowpassPole #-}
{-# INLINE butterworthHighpassPole #-}
{-# INLINE chebyshevALowpassPole #-}
{-# INLINE chebyshevAHighpassPole #-}
{-# INLINE chebyshevBLowpassPole #-}
{-# INLINE chebyshevBHighpassPole #-}

butterworthLowpassPole, butterworthHighpassPole,
   chebyshevALowpassPole, chebyshevAHighpassPole,
   chebyshevBLowpassPole, chebyshevBHighpassPole ::
   (Trans.C q, Module.C q yv, Dim.C u) =>
   NonNeg.Int   {- ^ Order of the filter, must be even,
                     the higher the order, the sharper is the separation of frequencies. -}  ->
   ResonantFilter s u q (FiltRec.Pole q) amp yv yv

butterworthLowpassPole  = higherOrderNoResoGenPole Butter.lowpassCausalPole
butterworthHighpassPole = higherOrderNoResoGenPole Butter.highpassCausalPole
chebyshevALowpassPole   = higherOrderNoResoGenPole Cheby.lowpassACausalPole
chebyshevAHighpassPole  = higherOrderNoResoGenPole Cheby.highpassACausalPole
chebyshevBLowpassPole   = higherOrderNoResoGenPole Cheby.lowpassBCausalPole
chebyshevBHighpassPole  = higherOrderNoResoGenPole Cheby.highpassBCausalPole


{- ToDo:
initial value

Here we use the filter frequency as filter parameter.
This simplifies interpolation of filter parameters
but means, that the low-level filter coefficients for filter cascade
must be computed at audio sample rate.
-}
{-# INLINE higherOrderNoResoGenPole #-}
higherOrderNoResoGenPole ::
   (Field.C q, Dim.C u) =>
   (Int -> Causal.T (FiltRec.Pole q, yv) yv) ->
   NonNeg.Int ->
   ResonantFilter s u q (FiltRec.Pole q) amp yv yv

higherOrderNoResoGenPole filt order =
   let orderInt = NonNeg.toNumber order
   in  frequencyResonanceControl id (filt orderInt)
-}


{-# INLINE highpassFromUniversal #-}
{-# INLINE bandpassFromUniversal #-}
{-# INLINE lowpassFromUniversal #-}
{-# INLINE bandlimitFromUniversal #-}
highpassFromUniversal, lowpassFromUniversal,
  bandpassFromUniversal, bandlimitFromUniversal ::
   CausalD.Single s amp amp (UniFilter.Result yv) yv
--   Proc.T s u q (CausalD.T s amp amp (UniFilter.Result yv) yv)
highpassFromUniversal  = homogeneousMap UniFilter.highpass
bandpassFromUniversal  = homogeneousMap UniFilter.bandpass
lowpassFromUniversal   = homogeneousMap UniFilter.lowpass
bandlimitFromUniversal = homogeneousMap UniFilter.bandlimit


-- we could also use Amp.Abstract, but this would yield an orphan instance for CProc.C
data UniversalGlobal = UniversalGlobal

{-# INLINE universal #-}
universal ::
   (Dim.C u, Trans.C q, Arrow arrow) =>
   Proc.T s u q
      (ArrowD.T arrow
         (Sample.Dimensional Dim.Scalar q q,
          -- Sample.Flat q,
          Sample.Dimensional (Dim.Recip u) q q)
         (Sample.T UniversalGlobal (CCProc.RateDep s (UniFilter.Parameter q))))
universal =
   flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
      (ArrowD.Cons $ \ (resoAmp, Amp.Numeric freqAmp) ->
         swap $
         (UniversalGlobal,
          let k = toFreq freqAmp
          in  arr $
              \ (reso, freq) ->
                  CCProc.RateDep $
                  UniFilter.parameter $
                  FiltRec.Pole (Flat.amplifySample resoAmp reso) (k*freq)))


instance Amp.C UniversalGlobal where
instance Amp.Primitive UniversalGlobal where primitive = UniversalGlobal

instance (Module.C q yv) =>
   CCProc.C UniversalGlobal (UniFilter.Parameter q)
      (Sample.T amp yv) (Sample.T amp (UniFilter.Result yv)) where
   process =
      return $ CausalD.consFlip $ \ (UniversalGlobal, amp) ->
         (amp, UniFilter.causal <<^ mapFst CCProc.unRateDep)


newtype MoogLowpassGlobal = MoogLowpassGlobal Int


{- |
The returned arrow has intentionally no @s@ type parameter,
in order to let you apply the parameter generator
to control signals with control sampling rate
that is different from the one target audio sampling rate.
-}
{-# INLINE moogLowpass #-}
moogLowpass ::
   (Dim.C u, Trans.C q, Arrow arrow) =>
   NonNeg.Int ->
   Proc.T s u q
      (ArrowD.T arrow
         (Sample.Dimensional Dim.Scalar q q,
          -- Sample.Flat q,
          Sample.Dimensional (Dim.Recip u) q q)
         (Sample.T MoogLowpassGlobal (CCProc.RateDep s (Moog.Parameter q))))
moogLowpass order =
   let orderInt = NonNeg.toNumber order
   in  flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
          ArrowD.Cons $ \ (resoAmp, Amp.Numeric freqAmp) ->
             swap $
             (MoogLowpassGlobal orderInt,
              let k = toFreq freqAmp
              in  arr $
                  \ (reso, freq) ->
                      CCProc.RateDep $
                      Moog.parameter orderInt $
                      FiltRec.Pole (Flat.amplifySample resoAmp reso) (k*freq))

instance Amp.C MoogLowpassGlobal where

instance (Module.C q yv) =>
   CCProc.C MoogLowpassGlobal (Moog.Parameter q)
      (Sample.T amp yv) (Sample.T amp yv) where
   process =
      return $ CausalD.consFlip $ \ (MoogLowpassGlobal orderInt, amp) ->
         (amp, Moog.lowpassCausal orderInt <<^ mapFst CCProc.unRateDep)


newtype AllpassCascadeGlobal = AllpassCascadeGlobal Int

{-# INLINE allpassCascade #-}
allpassCascade ::
   (Dim.C u, Trans.C q, Arrow arrow) =>
   NonNeg.Int  {- ^ order, number of filters in the cascade -} ->
   q           {- ^ the phase shift to be achieved for the given frequency -} ->
   Proc.T s u q
      (ArrowD.T arrow
         (Sample.Dimensional (Dim.Recip u) q q)
         (Sample.T AllpassCascadeGlobal (CCProc.RateDep s (Allpass.Parameter q))))
allpassCascade order phase =
   let orderInt = NonNeg.toNumber order
   in  flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
          ArrowD.Cons $ \ (Amp.Numeric freqAmp) ->
             swap $
             (AllpassCascadeGlobal orderInt,
              arr $
              \ freq ->
                  CCProc.RateDep $
                  Allpass.cascadeParameter orderInt phase $
                  freq * toFreq freqAmp)


instance Amp.C AllpassCascadeGlobal where

instance (Module.C q yv) =>
   CCProc.C AllpassCascadeGlobal (Allpass.Parameter q)
      (Sample.T amp yv) (Sample.T amp yv) where
   process =
      return $ CausalD.consFlip $ \ (AllpassCascadeGlobal orderInt, amp) ->
         (amp, Allpass.cascadeCausal orderInt <<^ mapFst CCProc.unRateDep)


newtype AllpassPhaserGlobal = AllpassPhaserGlobal Int

{-# INLINE allpassPhaser #-}
allpassPhaser ::
   (Dim.C u, Trans.C q, Arrow arrow) =>
   NonNeg.Int  {- ^ order, number of filters in the cascade -} ->
   Proc.T s u q
      (ArrowD.T arrow
         (Sample.Dimensional Dim.Scalar q q,
          -- Sample.Flat q,
          Sample.Dimensional (Dim.Recip u) q q)
         (Sample.T AllpassPhaserGlobal (CCProc.RateDep s (q, Allpass.Parameter q))))
allpassPhaser order =
   let orderInt = NonNeg.toNumber order
   in  flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
          ArrowD.Cons $ \ (resoAmp, Amp.Numeric freqAmp) ->
             swap $
             (AllpassPhaserGlobal orderInt,
              arr $
              \ (reso, freq) ->
                  CCProc.RateDep $
                  (Flat.amplifySample resoAmp reso,
                   Allpass.flangerParameter orderInt $
                   freq * toFreq freqAmp))


instance Amp.C AllpassPhaserGlobal where

instance (Module.C q yv) =>
   CCProc.C AllpassPhaserGlobal (q, Allpass.Parameter q)
      (Sample.T amp yv) (Sample.T amp yv) where
   process =
      return $ CausalD.consFlip $ \ (AllpassPhaserGlobal orderInt, amp) ->
         (amp,
          uncurry affineComb
          ^<<
          Causal.second (Causal.fanout
             (Allpass.cascadeCausal orderInt) (Causal.map snd))
          <<^
          (\(CCProc.RateDep (r,p), x) -> (r,(p,x))))


homogeneousMap ::
   (yv0 -> yv1) ->
   CausalD.Single s amp amp yv0 yv1
--   Proc.T s u t (CausalD.T s amp amp yv0 yv1)
homogeneousMap f =
   CausalD.homogeneous (Causal.map f)
--   Proc.pure (CausalD.homogeneous (Causal.map f))
