{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Signals equipped with volume and sample rate information that may carry a unit.
Kind of volume and sample rate is configurable by types.
-}
module Synthesizer.Dimensional.Signal.Private where

import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.Dimensional.Process as Proc
import Synthesizer.Dimensional.Process (($#), )

import qualified Synthesizer.Generic.Filter.NonRecursive as FiltG
import qualified Synthesizer.Generic.Signal as SigG

-- import qualified Data.StorableVector.Lazy.Pattern as SVP
import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Frame.Stereo as Stereo
import qualified Synthesizer.Basic.Binary as BinSmp
import Data.Int (Int16, )
import Foreign.Storable (Storable, )

import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.Module         as Module
import qualified Algebra.RealRing      as RealRing
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim


-- import NumericPrelude.Numeric
import NumericPrelude.Base as P
import Prelude ()


{- |
A signal value 0.5 at global amplitude 1
and signal value 1 at global amplitude 0.5
shall represent the same signal.
Thus observing the amplitude breaks the abstraction.

Cyclic nature such as needed for Fourier transform
must be expressend in the body.
It would be nice to use the data type for waveforms, too,
but for waveforms the @rate@ parameter makes no sense.
-}
data T rate amplitude body =
   Cons {
      sampleRate :: rate,
      amplitude :: amplitude,
      body :: body
   }

type R s v y yv = T (Rate.Phantom s) (Amp.Dimensional v y) (Sig.T yv)


{-# INLINE actualSampleRate #-}
actualSampleRate ::
   T (Rate.Actual rate) amp sig -> rate
actualSampleRate sig =
   let (Rate.Actual amp) = sampleRate sig
   in  amp

{-# INLINE actualAmplitude #-}
actualAmplitude ::
   T rate (Amp.Numeric amp) sig -> amp
actualAmplitude sig =
   let (Amp.Numeric amp) = amplitude sig
   in  amp


{-# INLINE toAmplitudeScalar #-}
toAmplitudeScalar :: (Field.C y, Dim.C v) =>
   T rate (Amp.Dimensional v y) sig -> DN.T v y -> y
toAmplitudeScalar sig y =
   DN.divToScalar y (actualAmplitude sig)

{-# INLINE rewriteAmplitudeDimension #-}
rewriteAmplitudeDimension :: (Dim.C v0, Dim.C v1) =>
   (v0 -> v1) ->
   T rate (Amp.Dimensional v0 y) sig ->
   T rate (Amp.Dimensional v1 y) sig
rewriteAmplitudeDimension f (Cons rate (Amp.Numeric amp) ss) =
   Cons rate (Amp.Numeric $ DN.rewriteDimension f amp) ss

{-# INLINE asTypeOfAmplitude #-}
asTypeOfAmplitude :: y -> T rate (Amp.Dimensional v y) sig -> y
asTypeOfAmplitude = const



{-# INLINE scalarSamples #-}
scalarSamples ::
   (Ring.C y, SigG.Transform sig y) =>
   (amp -> y) -> T rate (Amp.Numeric amp) (sig y) -> sig y
scalarSamples toAmpScalar sig =
   let y = toAmpScalar (actualAmplitude sig)
   in  FiltG.amplify y (body sig)

{-# INLINE vectorSamples #-}
vectorSamples ::
   (Module.C y yv, SigG.Transform sig yv) =>
   (amp -> y) -> T rate (Amp.Numeric amp) (sig yv) -> sig yv
vectorSamples toAmpScalar sig =
   let y = toAmpScalar (actualAmplitude sig)
   in  FiltG.amplifyVector y (body sig)


{-# INLINE embedSampleRate #-}
embedSampleRate :: (Dim.C u) =>
   Proc.T s u t
      (T (Rate.Phantom s) amp sig ->
       T (Rate.Dimensional u t) amp sig)
embedSampleRate =
   fmap
      (\rate (Cons _ amp sig) -> Cons (Rate.Actual rate) amp sig)
      Proc.getSampleRate

{-# INLINE render #-}
render :: (Dim.C u) =>
   DN.T (Dim.Recip u) t ->
   (forall s. Proc.T s u t (T (Rate.Phantom s) amp sig)) ->
   T (Rate.Dimensional u t) amp sig
render rate signal =
   Proc.run rate (embedSampleRate Proc.$: signal)

{-# INLINE apply #-}
apply :: (Dim.C u) =>
   (forall s. Proc.T s u t (T (Rate.Phantom s) amp0 sig0 -> T (Rate.Phantom s) amp1 sig1)) ->
   T (Rate.Dimensional u t) amp0 sig0 -> T (Rate.Dimensional u t) amp1 sig1
apply p x =
   render
      (actualSampleRate x)
      (p $# Cons Rate.Phantom (amplitude x) (body x))


{-
Zip heterogenous signals.
This yields a signal with mixed amplitudes,
e.g. @T rate (amp0, amp1) (Sig.T (y0,y1))@
and is consistent with the way
@Causal@ and @Wave.Controlled@ handle multiple sources.
However, it may be dangerous, since @T rate amp (Sig.T (y0,y1))@
might be used for stereo signals.
Of course, for stereo signals @Stereo.T@ should be prefered.
-}
zip ::
   (SigG.Transform sig y1, SigG.Transform sig (y0,y1), SigG.Read sig y0) =>
   T (Rate.Phantom s) amp0 (sig y0) ->
   T (Rate.Phantom s) amp1 (sig y1) ->
   T (Rate.Phantom s) (amp0,amp1) (sig (y0,y1))
zip x y =
   Cons
      Rate.Phantom
      (amplitude x, amplitude y)
      (SigG.zip (body x) (body y))


{-# INLINE processBody #-}
processBody ::
   (sig0 -> sig1) ->
   T rate amp sig0 ->
   T rate amp sig1
processBody f (Cons rate amp sig) =
   Cons rate amp (f sig)

{-# INLINE replaceBody #-}
replaceBody ::
   sig1 ->
   T rate amp sig0 ->
   T rate amp sig1
replaceBody sig =
   processBody (const sig)

{-# INLINE fromBody #-}
fromBody ::
   amp -> sig -> T (Rate.Phantom s) (Amp.Numeric amp) sig
fromBody amp =
   Cons Rate.Phantom (Amp.Numeric amp)

{-# INLINE flatFromBody #-}
flatFromBody ::
   sig -> T (Rate.Phantom s) (Amp.Flat y) sig
flatFromBody =
   Cons Rate.Phantom Amp.Flat

{-# INLINE abstractFromBody #-}
abstractFromBody ::
   sig -> T (Rate.Phantom s) Amp.Abstract sig
abstractFromBody =
   Cons Rate.Phantom Amp.Abstract

{-# INLINE primitiveFromBody #-}
primitiveFromBody ::
   (Amp.Primitive amp) =>
   sig -> T (Rate.Phantom s) amp sig
primitiveFromBody =
   Cons Rate.Phantom Amp.primitive


-- * caching

{-# INLINE cache #-}
cache ::
   (Storable yv) =>
   T rate amp (Sig.T yv) ->
   T rate amp (Sig.T yv)
cache =
   processBody
      (Sig.fromStorableSignal . Sig.toStorableSignal defaultChunkSize)

{-# INLINE bindCached #-}
bindCached ::
   (Storable yv) =>
   Proc.T s u t (T rate amp (Sig.T yv)) ->
   (T rate amp (Sig.T yv) -> Proc.T s u t b) ->
   Proc.T s u t b
bindCached x y =
   y . cache =<< x

{-# INLINE share #-}
share ::
   (Storable yv) =>
   Proc.T s u t (T rate amp (Sig.T yv)) ->
   (Proc.T s u t (T rate amp (Sig.T yv)) -> Proc.T s u t b) ->
   Proc.T s u t b
share x y = bindCached x (y . return)



{-# INLINE store #-}
store ::
   (RealRing.C t, Dim.C u, Storable yv) =>
   DN.T u t ->
   Proc.T s u t (
      {-
      Rate.Phantom required,
      because chunk size is dicretized with respect to the process' sample rate
      -}
      T (Rate.Phantom s) amp (Sig.T yv) ->
      T (Rate.Phantom s) amp (SigSt.T yv))
store chunkSize =
   fmap
      (\cs -> processBody (Sig.toStorableSignal (SigSt.chunkSize cs)))
      (Proc.intFromTime "Dimensional.Signal.store" chunkSize)

{-
better use ChunkySize.Signal.store
we do not need Proc context
{-# INLINE storeTake #-}
storeTake ::
   (RealRing.C t, Dim.C u, Storable yv) =>
   Proc.T s u t (
      T (Rate.Phantom s) Amp.Abstract SVP.LazySize ->
      T (Rate.Phantom s) amp (Sig.T yv) ->
      T (Rate.Phantom s) amp (SigSt.T yv))
storeTake =
   return
      (\cs -> processBody (Sig.toStorableSignalVary (body cs)))
-}

{-# INLINE restore #-}
restore ::
   (SigG.Read sig yv) =>
   T rate amp (sig yv) ->
   T rate amp (Sig.T yv)
restore =
   processBody SigG.toState

{-
{-# INLINE restore #-}
restore ::
   (Storable yv) =>
   T rate amp (SigSt.T yv) ->
   T rate amp (Sig.T yv)
restore =
   processBody Sig.fromStorableSignal
-}



{-# INLINE toStorableInt16Mono #-}
toStorableInt16Mono ::
   (RealRing.C a) =>
   T rate (Amp.Dimensional Dim.Voltage a) (Sig.T a) ->
   SigSt.T Int16
toStorableInt16Mono =
   Sig.toStorableSignal defaultChunkSize .
   Sig.map BinSmp.int16FromCanonical .
   scalarSamples (DN.toNumberWithDimension Dim.voltage)

{-# INLINE toStorableInt16Stereo #-}
toStorableInt16Stereo ::
   (Module.C a a, RealRing.C a) =>
   T rate (Amp.Dimensional Dim.Voltage a) (Sig.T (Stereo.T a)) ->
   SigSt.T (Stereo.T Int16)
toStorableInt16Stereo =
   Sig.toStorableSignal defaultChunkSize .
   Sig.map (Stereo.map BinSmp.int16FromCanonical) .
   vectorSamples (DN.toNumberWithDimension Dim.voltage)


defaultChunkSize :: SigSt.ChunkSize
defaultChunkSize =
--   SigSt.chunkSize 131072
   SigSt.defaultChunkSize
