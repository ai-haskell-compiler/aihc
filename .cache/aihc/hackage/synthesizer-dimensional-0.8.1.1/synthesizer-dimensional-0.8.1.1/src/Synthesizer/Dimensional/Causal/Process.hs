{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Synthesizer.Dimensional.Causal.Process (
   module Synthesizer.Dimensional.Causal.Process,

   -- * re-export Arrow, it would be better to restrict that to Causal processes
   (Arrow.***), (Arrow.&&&),
   (Arrow.>>>), (Arrow.<<<),

   ArrowD.compose,
   ArrowD.first,
   ArrowD.second,
   ArrowD.split,
   ArrowD.fanout,
   ArrowD.loop,
   ArrowD.loopVolume,
   ) where

import qualified Synthesizer.Dimensional.Arrow as ArrowD
import qualified Synthesizer.Dimensional.Map as Map

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.Causal.Arrow as CausalArrow
import qualified Synthesizer.Causal.Process as Causal
import qualified Control.Arrow as Arrow
import Control.Arrow (Arrow, ArrowLoop, first, (>>>), (<<<), )
import Control.Category (Category, )

import Control.Applicative (Applicative, )

import qualified Synthesizer.State.Signal as Sig
import qualified Synthesizer.Generic.Signal  as SigG

import qualified Algebra.Module as Module
import qualified Algebra.Field  as Field
import qualified Algebra.Ring   as Ring

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Data.Tuple.HT as TupleHT (mapFst, )

-- import NumericPrelude.Numeric (one)
import Prelude hiding (map, id, fst, snd, )



{- |
Note that @amp@ can also be a pair of amplitudes
or a more complicated ensemble of amplitudes.
-}
type T s sample0 sample1 =
   ArrowD.T (Core s) sample0 sample1

type Single s amp0 amp1 yv0 yv1 =
   ArrowD.Single (Core s) amp0 amp1 yv0 yv1

newtype Core s yv0 yv1 =
   Core (Causal.T yv0 yv1)
   deriving (Category, Arrow, ArrowLoop, CausalArrow.C)

instance ArrowD.Applicable (Core s) (Rate.Phantom s)


consFlip ::
   (Sample.Amplitude sample0 ->
    (Sample.Amplitude sample1,
     Causal.T (Sample.Displacement sample0)
              (Sample.Displacement sample1))) ->
   T s sample0 sample1
consFlip f =
   ArrowD.Cons $ \ampIn ->
      let (ampOut, causal) = f ampIn
      in  (Core causal, ampOut)


infixl 9 `apply`

{-# INLINE apply #-}
apply ::
   (SigG.Transform sig yv0, SigG.Transform sig yv1) =>
   Single s amp0 amp1 yv0 yv1 ->
   SigA.T (Rate.Phantom s) amp0 (sig yv0) ->
   SigA.T (Rate.Phantom s) amp1 (sig yv1)
apply = ArrowD.apply

{-# INLINE applyFlat #-}
applyFlat ::
   (Flat.C yv0 amp0,
    SigG.Transform sig yv0, SigG.Transform sig yv1) =>
   Single s (Amp.Flat yv0) amp1 yv0 yv1 ->
   SigA.T (Rate.Phantom s) amp0 (sig yv0) ->
   SigA.T (Rate.Phantom s) amp1 (sig yv1)
applyFlat = ArrowD.applyFlat

{-# INLINE canonicalizeFlat #-}
canonicalizeFlat ::
   (Flat.C y flat) =>
   Single s flat (Amp.Flat y) y y
canonicalizeFlat =
   ArrowD.canonicalizeFlat


{-# INLINE applyConst #-}
applyConst ::
   (Amp.C amp1, Ring.C y0) =>
   Single s (Amp.Numeric amp0) amp1 y0 yv1 ->
   amp0 ->
   SigA.T (Rate.Phantom s) amp1 (Sig.T yv1)
applyConst = ArrowD.applyConst



infixl 0 $/:, $/-

{-# INLINE ($/:) #-}
($/:) ::
   (Applicative f,
    SigG.Transform sig yv0, SigG.Transform sig yv1) =>
   f (Single s amp0 amp1 yv0 yv1) ->
   f (SigA.T (Rate.Phantom s) amp0 (sig yv0)) ->
   f (SigA.T (Rate.Phantom s) amp1 (sig yv1))
($/:) = (ArrowD.$/:)

{-# INLINE ($/-) #-}
($/-) ::
   (Amp.C amp1, Functor f, Ring.C y0) =>
   f (Single s (Amp.Numeric amp0) amp1 y0 yv1) ->
   amp0 ->
   f (SigA.T (Rate.Phantom s) amp1 (Sig.T yv1))
($/-) = (ArrowD.$/-)



infixl 9 `applyFst`

{-# INLINE applyFst #-}
applyFst ::
   (SigG.Read sig yv) =>
   T s (Sample.T amp yv, restSampleIn) restSampleOut ->
   SigA.T (Rate.Phantom s) amp (sig yv) ->
   T s restSampleIn restSampleOut
applyFst c x = c <<< feedFst x

{-# INLINE applyFlatFst #-}
applyFlatFst ::
   (Flat.C yv amp, SigG.Read sig yv) =>
   T s (Sample.T (Amp.Flat yv) yv, restSampleIn) restSampleOut ->
   SigA.T (Rate.Phantom s) amp (sig yv) ->
   T s restSampleIn restSampleOut
applyFlatFst c =
   applyFst (c <<< first canonicalizeFlat)


{-# INLINE feedFst #-}
feedFst ::
   (SigG.Read sig yv) =>
   SigA.T (Rate.Phantom s) amp (sig yv) ->
   T s restSample (Sample.T amp yv, restSample)
feedFst x =
   ArrowD.Cons $ \yAmp ->
      (Core $ Causal.feedFst (SigA.body x), (SigA.amplitude x, yAmp))


{-# INLINE applySnd #-}
applySnd ::
   (SigG.Read sig yv) =>
   T s (restSampleIn, Sample.T amp yv) restSampleOut ->
   SigA.T (Rate.Phantom s) amp (sig yv) ->
   T s restSampleIn restSampleOut
applySnd c x = c <<< feedSnd x

{-# INLINE feedSnd #-}
feedSnd ::
   (SigG.Read sig yv) =>
   SigA.T (Rate.Phantom s) amp (sig yv) ->
   T s restSample (restSample, Sample.T amp yv)
feedSnd x =
   ArrowD.Cons $ \yAmp ->
      (Core $ Causal.feedSnd (SigA.body x), (yAmp, SigA.amplitude x))


{-# INLINE map #-}
map ::
   Map.T sample0 sample1 ->
   T s sample0 sample1
map (ArrowD.Cons f) =
   ArrowD.Cons $ mapFst Arrow.arr . f


infixr 1 ^>>, >>^
infixr 1 ^<<, <<^

{-# INLINE (^>>) #-}
-- | Precomposition with a pure function.
(^>>) ::
   Map.T sample0 sample1 ->
   T s sample1 sample2 ->
   T s sample0 sample2
f ^>> a = map f >>> a

{-# INLINE (>>^) #-}
-- | Postcomposition with a pure function.
(>>^) ::
   T s sample0 sample1 ->
   Map.T sample1 sample2 ->
   T s sample0 sample2
a >>^ f = a >>> map f

{-# INLINE (<<^) #-}
-- | Precomposition with a pure function (right-to-left variant).
(<<^) ::
   T s sample1 sample2 ->
   Map.T sample0 sample1 ->
   T s sample0 sample2
a <<^ f = a <<< map f

{-# INLINE (^<<) #-}
-- | Postcomposition with a pure function (right-to-left variant).
(^<<) ::
   Map.T sample1 sample2 ->
   T s sample0 sample1 ->
   T s sample0 sample2
f ^<< a = map f <<< a


{- |
Lift a low-level homogeneous process to a dimensional one.

Note that the @amp@ type variable is unrestricted.
This way we show, that the amplitude is not touched,
which also means that the underlying low-level process must be homogeneous.
-}
{-# INLINE homogeneous #-}
homogeneous ::
   Causal.T yv0 yv1 ->
   Single s amp amp yv0 yv1
homogeneous c =
   ArrowD.Cons $ \ xAmp -> (Core c, xAmp)


{-# INLINE id #-}
id ::
   T s sample sample
id =
   ArrowD.id


{-# INLINE loop2Volume #-}
loop2Volume ::
   (Field.C y0, Module.C y0 yv0, Dim.C v0,
    Field.C y1, Module.C y1 yv1, Dim.C v1) =>
   (DN.T v0 y0, DN.T v1 y1) ->
   T s
     (restSampleIn,  (Sample.T (Amp.Dimensional v0 y0) yv0,
                      Sample.T (Amp.Dimensional v1 y1) yv1))
     (restSampleOut, (Sample.T (Amp.Dimensional v0 y0) yv0,
                      Sample.T (Amp.Dimensional v1 y1) yv1)) ->
   T s restSampleIn restSampleOut
loop2Volume (amp0,amp1) p =
   ArrowD.loopVolume amp0 $
   ArrowD.loopVolume amp1 $
   (Map.balanceRight >>> p >>> Map.balanceLeft)
-- alternative implementation to ArrowD.loop2Volume
