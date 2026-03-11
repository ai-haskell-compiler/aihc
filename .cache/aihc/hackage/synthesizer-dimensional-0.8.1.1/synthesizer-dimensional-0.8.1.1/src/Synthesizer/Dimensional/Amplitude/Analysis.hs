{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Amplitude.Analysis (
    beginning, end,
    beginningPrimitive, endPrimitive,

    volumeMaximum,
    volumeEuclidean,
    volumeSum,
    volumeVectorMaximum,
    volumeVectorEuclidean,
    volumeVectorSum,

    directCurrentOffset,
    rectify,
    flipFlopHysteresis,

    compare,
    lessOrEqual,
  ) where

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Amplitude.Cut as CutD
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.State.Analysis as Ana
import qualified Synthesizer.State.Signal   as Sig
import Synthesizer.Plain.Analysis (BinaryLevel)

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((*&))

import qualified Algebra.NormedSpace.Maximum   as NormedMax
import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.NormedSpace.Sum       as NormedSum

import qualified Algebra.Algebraic           as Algebraic
import qualified Algebra.Module              as Module
import qualified Algebra.Field               as Field
import qualified Algebra.Absolute            as Absolute
import qualified Algebra.RealRing            as RealRing
import qualified Algebra.Ring                as Ring
import qualified Algebra.Additive            as Additive


import NumericPrelude.Base (Ord, Bool, (<=), ($), uncurry, )
-- import NumericPrelude.Numeric
import qualified Prelude as P



{- * Notions of volume -}

type SignalRateInd rate u y yv =
   SigA.T rate (Amp.Numeric (DN.T u y)) (Sig.T yv)

{-# INLINE beginning #-}
beginning ::
   (Ring.C y, Dim.C v, SigG.Transform sig y) =>
   SigA.T rate (Amp.Dimensional v y) (sig y) -> DN.T v y
beginning sig =
   SigG.switchL
--      (error "Dimensional.Analysis.beginning: empty signal")
      Additive.zero
      (\y _ -> DN.scale y $ SigA.actualAmplitude sig)
      (SigA.body sig)

{-# INLINE end #-}
end ::
   (Ring.C y, Dim.C v, SigG.Transform sig y) =>
   SigA.T rate (Amp.Dimensional v y) (sig y) -> DN.T v y
end sig =
   SigG.switchR
--      (error "Dimensional.Analysis.end: empty signal")
      Additive.zero
      (\_ y -> DN.scale y $ SigA.actualAmplitude sig)
      (SigA.body sig)


{-# INLINE beginningPrimitive #-}
beginningPrimitive ::
   (Amp.Primitive amp, SigG.Transform sig y) =>
   y -> SigA.T rate amp (sig y) -> y
beginningPrimitive deflt sig =
   SigG.switchL
      deflt
      (\y _ -> y)
      (SigA.body sig)

{-# INLINE endPrimitive #-}
endPrimitive ::
   (Amp.Primitive amp, SigG.Transform sig y) =>
   y -> SigA.T rate amp (sig y) -> y
endPrimitive deflt sig =
   SigG.switchR
      deflt
      (\_ y -> y)
      (SigA.body sig)


{- |
Volume based on Manhattan norm.
-}
{-# INLINE volumeMaximum #-}
volumeMaximum :: (RealRing.C y, Dim.C u) =>
   SignalRateInd rate u y y -> DN.T u y
volumeMaximum = volumeAux Ana.volumeMaximum

{- |
Volume based on Energy norm.
-}
{-# INLINE volumeEuclidean #-}
volumeEuclidean :: (Algebraic.C y, Dim.C u) =>
   SignalRateInd rate u y y -> DN.T u y
volumeEuclidean = volumeAux Ana.volumeEuclidean

{- |
Volume based on Sum norm.
-}
{-# INLINE volumeSum #-}
volumeSum :: (Field.C y, Absolute.C y, Dim.C u) =>
   SignalRateInd rate u y y -> DN.T u y
volumeSum = volumeAux Ana.volumeSum



{- |
Volume based on Manhattan norm.
-}
{-# INLINE volumeVectorMaximum #-}
volumeVectorMaximum :: (NormedMax.C y yv, Ord y, Dim.C u) =>
   SignalRateInd rate u y yv -> DN.T u y
volumeVectorMaximum = volumeAux Ana.volumeVectorMaximum

{- |
Volume based on Energy norm.
-}
{-# INLINE volumeVectorEuclidean #-}
volumeVectorEuclidean :: (NormedEuc.C y yv, Algebraic.C y, Dim.C u) =>
   SignalRateInd rate u y yv -> DN.T u y
volumeVectorEuclidean = volumeAux Ana.volumeVectorEuclidean

{- |
Volume based on Sum norm.
-}
{-# INLINE volumeVectorSum #-}
volumeVectorSum :: (NormedSum.C y yv, Field.C y, Dim.C u) =>
   SignalRateInd rate u y yv -> DN.T u y
volumeVectorSum = volumeAux Ana.volumeVectorSum


{-# INLINE volumeAux #-}
volumeAux :: (Ring.C y, Dim.C u) =>
   (Sig.T yv -> y) -> SignalRateInd rate u y yv -> DN.T u y
volumeAux vol x =
   vol (SigA.body x) *& SigA.actualAmplitude x


{- * Miscellaneous -}

{- |
Requires finite length.
This is identical to the arithmetic mean.
-}
{-# INLINE directCurrentOffset #-}
directCurrentOffset :: (Field.C y, Dim.C u) =>
   SignalRateInd rate u y y -> DN.T u y
directCurrentOffset =
   volumeAux Ana.directCurrentOffset

{-# INLINE rectify #-}
rectify :: (Absolute.C y) =>
   SigA.T rate amp (Sig.T y) -> SigA.T rate amp (Sig.T y)
rectify = SigA.processBody Ana.rectify


{- |
Detect thresholds with a hysteresis.
-}
{-# INLINE flipFlopHysteresis #-}
flipFlopHysteresis :: (Ord y, Field.C y, Dim.C u) =>
   (DN.T u y, DN.T u y) -> BinaryLevel ->
   SignalRateInd rate u y y ->
   SigA.T rate Amp.Abstract (Sig.T BinaryLevel)
flipFlopHysteresis (lower,upper) start x =
   let l = SigA.toAmplitudeScalar x lower
       h = SigA.toAmplitudeScalar x upper
   in  SigA.Cons (SigA.sampleRate x) Amp.Abstract $
       Ana.flipFlopHysteresis (l,h) start $
       SigA.body x


{- * comparison -}

{-# INLINE compare #-}
compare ::
   (Ord y, Field.C y, Dim.C u,
    Module.C y yv, Ord yv) =>
   SigA.R s u y yv ->
   SigA.R s u y yv ->
   SigA.T (Rate.Phantom s) Amp.Abstract (Sig.T P.Ordering)
compare x y =
   SigA.Cons Rate.Phantom Amp.Abstract $
   Sig.map (uncurry P.compare) $ SigA.body $ CutD.zip x y

{-# INLINE lessOrEqual #-}
lessOrEqual ::
   (Ord y, Field.C y, Dim.C u,
    Module.C y yv, Ord yv) =>
   SigA.R s u y yv ->
   SigA.R s u y yv ->
   SigA.T (Rate.Phantom s) Amp.Abstract (Sig.T Bool)
lessOrEqual x y =
   SigA.processBody (Sig.map (<= P.EQ)) $ compare x y
