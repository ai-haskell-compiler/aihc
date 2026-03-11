{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.RateAmplitude.Cut (
   {- * dissection -}
   splitAt,
   take,
   drop,
   takeUntilPause,
   unzip,
   unzip3,
   leftFromStereo, rightFromStereo,

   {- * glueing -}
   concat,      concatVolume,
   append,      appendVolume,
   zip,         zipVolume,
   zip3,        zip3Volume,
   mergeStereo, mergeStereoVolume,
   arrange,     arrangeVolume,
   arrangeStorableVolume,
  ) where

import qualified Synthesizer.Dimensional.Amplitude.Cut as CutV
import qualified Synthesizer.Dimensional.Rate.Cut as CutR
import qualified Synthesizer.Storable.Cut as CutSt
import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.State.Cut as CutS
import qualified Synthesizer.State.Signal as Sig
import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.Frame.Stereo as Stereo
import Foreign.Storable (Storable, )

import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc
import Synthesizer.Dimensional.Process (($#), toTimeScalar, intFromTime98, )
import Synthesizer.Dimensional.Signal.Private (toAmplitudeScalar, )

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

-- import Number.DimensionTerm ((&*&))

import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified Algebra.NormedSpace.Maximum as NormedMax
import qualified Algebra.Module              as Module
import qualified Algebra.RealRing           as RealRing
import qualified Algebra.Field               as Field
import qualified Algebra.Ring                as Ring

import qualified Data.List as List

import NumericPrelude.Base ((.), ($), Ord, (<=), map, return, )
-- import NumericPrelude.Numeric
import Prelude (RealFrac, )


{- * dissection -}

{-# INLINE splitAt #-}
splitAt :: (RealRing.C t, Dim.C u, Dim.C v, Storable yv) =>
   DN.T u t -> Proc.T s u t (SigA.R s v y yv -> (SigA.R s v y yv, SigA.R s v y yv))
splitAt t' =
   do t <- toTimeScalar t'
      return $ \x ->
         let (ss0,ss1) = Sig.splitAt (RealRing.round t) (SigA.body x)
         in  (SigA.replaceBody ss0 x,
              SigA.replaceBody ss1 x)

{-# INLINE take #-}
take :: (RealRing.C t, Dim.C u, Dim.C v) =>
   DN.T u t -> Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv)
take t' =
   CutR.take t'
   -- fmap (fst.) $ splitAt t

{-# INLINE drop #-}
drop :: (RealRing.C t, Dim.C u, Dim.C v) =>
   DN.T u t -> Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv)
drop t' =
   CutR.drop t'
   -- fmap (snd.) $ splitAt t

{-# INLINE takeUntilPause #-}
takeUntilPause ::
  (RealRing.C t, Dim.C u,
   Field.C y, NormedMax.C y yv, Dim.C v) =>
   DN.T v y -> DN.T u t -> Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv)
takeUntilPause y' t' =
   do t <- toTimeScalar t'
      return $ \x ->
         let y = toAmplitudeScalar x y'
         in  SigA.processBody
                (CutS.takeUntilInterval ((<=y) . NormedMax.norm)
                    (RealRing.ceiling t)) x


{-# INLINE unzip #-}
unzip :: (Dim.C u, Dim.C v) =>
   Proc.T s u t
      (SigA.R s v y (yv0, yv1) ->
       (SigA.R s v y yv0, SigA.R s v y yv1))
unzip = Proc.pure CutV.unzip

{-# INLINE unzip3 #-}
unzip3 :: (Dim.C u, Dim.C v) =>
   Proc.T s u t
      (SigA.R s v y (yv0, yv1, yv2) ->
       (SigA.R s v y yv0, SigA.R s v y yv1, SigA.R s v y yv2))
unzip3 = Proc.pure CutV.unzip3


{-# INLINE leftFromStereo #-}
leftFromStereo :: (Dim.C u) =>
   Proc.T s u t
      (SigA.R s u y (Stereo.T yv) -> SigA.R s u y yv)
leftFromStereo = Proc.pure CutV.leftFromStereo

{-# INLINE rightFromStereo #-}
rightFromStereo :: (Dim.C u) =>
   Proc.T s u t
      (SigA.R s u y (Stereo.T yv) -> SigA.R s u y yv)
rightFromStereo = Proc.pure CutV.rightFromStereo



{- * glueing -}

{- |
Similar to @foldr1 append@ but more efficient and accurate,
because it reduces the number of amplifications.
Does not work for infinite lists,
because no maximum amplitude can be computed.
-}
{-# INLINE concat #-}
concat ::
   (Ord y, Field.C y, Dim.C v, Dim.C u,
    Module.C y yv) =>
   Proc.T s u t ([SigA.R s v y yv] -> SigA.R s v y yv)
concat = Proc.pure $ CutV.concat

{- |
Give the output volume explicitly.
Does also work for infinite lists.
-}
{-# INLINE concatVolume #-}
concatVolume ::
   (Field.C y, Dim.C v, Dim.C u,
    Module.C y yv) =>
   DN.T v y -> Proc.T s u t ([SigA.R s v y yv] -> SigA.R s v y yv)
concatVolume amp = Proc.pure $ CutV.concatVolume amp


{-# INLINE append #-}
append ::
   (Ord y, Field.C y, Dim.C v, Dim.C u,
    Module.C y yv) =>
   Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv -> SigA.R s v y yv)
append = Proc.pure $ CutV.append

{-# INLINE appendVolume #-}
appendVolume ::
   (Field.C y, Dim.C v, Dim.C u,
    Module.C y yv) =>
   DN.T v y ->
   Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv -> SigA.R s v y yv)
appendVolume amp = Proc.pure $ CutV.appendVolume amp


{-# INLINE zip #-}
zip ::
   (Ord y, Field.C y, Dim.C v,
    Module.C y yv0, Module.C y yv1) =>
   Proc.T s u t (SigA.R s v y yv0 -> SigA.R s v y yv1 -> SigA.R s v y (yv0,yv1))
zip = Proc.pure $ CutV.zip

{-# INLINE zipVolume #-}
zipVolume ::
   (Field.C y, Dim.C v,
    Module.C y yv0, Module.C y yv1) =>
   DN.T v y ->
   Proc.T s u t (SigA.R s v y yv0 -> SigA.R s v y yv1 -> SigA.R s v y (yv0,yv1))
zipVolume amp = Proc.pure $ CutV.zipVolume amp


{-# INLINE mergeStereo #-}
mergeStereo ::
   (Ord y, Field.C y, Dim.C v,
    Module.C y yv) =>
   Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv -> SigA.R s v y (Stereo.T yv))
mergeStereo = Proc.pure $ CutV.mergeStereo

{-# INLINE mergeStereoVolume #-}
mergeStereoVolume ::
   (Field.C y, Dim.C v,
    Module.C y yv) =>
   DN.T v y ->
   Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv -> SigA.R s v y (Stereo.T yv))
mergeStereoVolume amp = Proc.pure $ CutV.mergeStereoVolume amp



{-# INLINE zip3 #-}
zip3 ::
   (Ord y, Field.C y, Dim.C v,
    Module.C y yv0, Module.C y yv1, Module.C y yv2) =>
   Proc.T s u t (
      SigA.R s v y yv0 -> SigA.R s v y yv1 -> SigA.R s v y yv2 ->
      SigA.R s v y (yv0,yv1,yv2))
zip3 = Proc.pure $ CutV.zip3

{-# INLINE zip3Volume #-}
zip3Volume ::
   (Field.C y, Dim.C v,
    Module.C y yv0, Module.C y yv1, Module.C y yv2) =>
   DN.T v y ->
   Proc.T s u t (
      SigA.R s v y yv0 -> SigA.R s v y yv1 -> SigA.R s v y yv2 ->
      SigA.R s v y (yv0,yv1,yv2))
zip3Volume amp = Proc.pure $ CutV.zip3Volume amp


{- |
Uses maximum input volume as output volume.
Does not work for infinite schedules,
because no maximum amplitude can be computed.
-}
{-# INLINE arrange #-}
arrange ::
   (Ring.C t, Dim.C u,
    RealFrac t,
    Ord y, Field.C y, Dim.C v,
    Module.C y yv, Storable yv) =>
      DN.T u t  {- ^ Maximum chunk size -}
   -> DN.T u t  {- ^ Unit of the time values in the time ordered list. -}
   -> Proc.T s u t (
         EventList.T (NonNeg.T t) (SigA.R s v y yv)
               {- v A list of pairs: (relative start time, signal part),
                    The start time is relative
                    to the start time of the previous event. -}
      -> SigA.R s v y yv)
               {- ^ The mixed signal. -}
arrange chunkSize unit' =
   Proc.withParam $ \sched ->
      let amp = List.maximum (map SigA.actualAmplitude (EventList.getBodies sched))
      in  arrangeVolume chunkSize amp unit' $# sched


{- |
Given a list of signals with time stamps,
mix them into one signal as they occur in time.
Ideal for composing music.
-}
{-# INLINE arrangeVolume #-}
arrangeVolume ::
   (Ring.C t, Dim.C u,
    RealFrac t,
    Field.C y, Dim.C v,
    Module.C y yv, Storable yv) =>
      DN.T u t  {- ^ Maximum chunk size -}
   -> DN.T v y  {- ^ Output volume -}
   -> DN.T u t  {- ^ Unit of the time values in the time ordered list. -}
   -> Proc.T s u t (
         EventList.T (NonNeg.T t) (SigA.R s v y yv)
            {- v A list of pairs: (relative start time, signal part),
                 The start time is relative
                 to the start time of the previous event. -}
      -> SigA.R s v y yv)
            {- ^ The mixed signal. -}
arrangeVolume chunkSize' amp unit' =
   do unit <- toTimeScalar unit'
      chunkSize <-
         intFromTime98 "Dimensional.Cut.arrangeStorableVolume" chunkSize'
      return $ \sched ->
         let z =
                SigA.fromBody amp $
                SigG.toState $
                CutSt.arrange (SigSt.chunkSize chunkSize) $ 
                EventList.resample
                   (NonNeg.fromNumberMsg "Dimensional.Cut.arrangeVolume" unit) $
                EventList.mapBody
                   (SigG.fromState (SigG.LazySize chunkSize) .
                    SigA.vectorSamples (toAmplitudeScalar z))
                sched
         in  z

{-# INLINE arrangeStorableVolume #-}
arrangeStorableVolume ::
   (Ring.C t, Dim.C u,
    RealFrac t,
    Field.C y, Dim.C v,
    Module.C y yv, Storable yv) =>
      DN.T u t  {- ^ Maximum chunk size -}
   -> DN.T v y  {- ^ Output volume -}
   -> DN.T u t  {- ^ Unit of the time values in the time ordered list. -}
   -> Proc.T s u t (
         EventList.T (NonNeg.T t)
            (SigA.T (Rate.Phantom s) (Amp.Dimensional v y) (SigSt.T yv))
            {- v A list of pairs: (relative start time, signal part),
                 The start time is relative
                 to the start time of the previous event. -}
      -> (SigA.T (Rate.Phantom s) (Amp.Dimensional v y) (SigSt.T yv)))
            {- ^ The mixed signal. -}
arrangeStorableVolume chunkSize' amp unit' =
   do unit <- toTimeScalar unit'
      chunkSize <-
         intFromTime98 "Dimensional.Cut.arrangeStorableVolume" chunkSize'
      return $ \sched ->
         let z =
                SigA.fromBody amp $
                CutSt.arrange (SigSt.chunkSize chunkSize) $
                EventList.resample
                   (NonNeg.fromNumberMsg "Dimensional.Cut.arrangeStorableVolume" unit) $
                EventList.mapBody
                   (SigA.vectorSamples (toAmplitudeScalar z))
                sched
         in  z
