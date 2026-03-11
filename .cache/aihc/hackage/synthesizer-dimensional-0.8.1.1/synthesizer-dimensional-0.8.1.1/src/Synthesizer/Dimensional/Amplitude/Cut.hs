{-# LANGUAGE FlexibleContexts #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Amplitude.Cut (
   -- * dissection
   unzip,
   unzip3,
   leftFromStereo, rightFromStereo,

   span, dropWhile, takeWhile,
   spanPrimitive, dropWhilePrimitive, takeWhilePrimitive,

   -- * glueing
   concat,      concatVolume,      concatPrimitive,
   append,      appendVolume,      appendPrimitive,
   zip,         zipVolume,
   zip3,        zip3Volume,
   mergeStereo, mergeStereoVolume, mergeStereoPrimitive,

   -- * miscellaneous
   selectBool,
   reverse,
  ) where

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import Synthesizer.Dimensional.Signal.Private (toAmplitudeScalar, )

import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Amplitude as Amp

import qualified Synthesizer.Generic.Signal  as SigG
import qualified Synthesizer.Generic.Cut     as CutG
import qualified Synthesizer.State.Signal    as Sig

import qualified Synthesizer.Frame.Stereo as Stereo

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

-- import Number.DimensionTerm ((&*&))

-- import qualified Algebra.NormedSpace.Maximum as NormedMax
import qualified Algebra.Module              as Module
import qualified Algebra.Field               as Field
-- import qualified Algebra.Ring                as Ring

import qualified Data.List as List

import NumericPrelude.Base (Ord, max, Bool, ($), (.), flip, )
import NumericPrelude.Numeric ((*>), )
import Prelude ()


-- * dissection

{-# INLINE unzip #-}
unzip ::
   (SigG.Transform sig (yv0, yv1),
    SigG.Transform sig yv0,
    SigG.Transform sig yv1) =>
   SigA.T rate amp (sig (yv0, yv1)) ->
   (SigA.T rate amp (sig yv0), SigA.T rate amp (sig yv1))
unzip x =
   let (ss0,ss1) = SigG.unzip (SigA.body x)
   in  (SigA.replaceBody ss0 x, SigA.replaceBody ss1 x)

{-# INLINE unzip3 #-}
unzip3 ::
   (SigG.Transform sig (yv0, yv1, yv2),
    SigG.Transform sig yv0,
    SigG.Transform sig yv1,
    SigG.Transform sig yv2) =>
   SigA.T rate amp (sig (yv0, yv1, yv2)) ->
   (SigA.T rate amp (sig yv0), SigA.T rate amp (sig yv1), SigA.T rate amp (sig yv2))
unzip3 x =
   let (ss0,ss1,ss2) = SigG.unzip3 (SigA.body x)
   in  (SigA.replaceBody ss0 x, SigA.replaceBody ss1 x, SigA.replaceBody ss2 x)


{-
ToDo:
spanNorm with a predicate with respect to a volume
would be useful in many cases.
But with respect to what notion of volume?
-}


span ::
   (SigG.Transform sig yv, Dim.C v, Field.C y, Module.C y yv) =>
   DN.T v y ->
   (yv -> Bool) ->
   (SigA.T rate (Amp.Dimensional v y) (sig yv) ->
    (SigA.T rate (Amp.Dimensional v y) (sig yv),
     SigA.T rate (Amp.Dimensional v y) (sig yv)))
span v p x =
   spanPrivate (p . (toAmplitudeScalar x v *>)) x

dropWhile ::
   (SigG.Transform sig yv, Dim.C v, Field.C y, Module.C y yv) =>
   DN.T v y ->
   (yv -> Bool) ->
   SigA.T rate (Amp.Dimensional v y) (sig yv) ->
   SigA.T rate (Amp.Dimensional v y) (sig yv)
dropWhile v p x =
   dropWhilePrivate (p . (toAmplitudeScalar x v *>)) x

takeWhile ::
   (SigG.Transform sig yv, Dim.C v, Field.C y, Module.C y yv) =>
   DN.T v y ->
   (yv -> Bool) ->
   SigA.T rate (Amp.Dimensional v y) (sig yv) ->
   SigA.T rate (Amp.Dimensional v y) (sig yv)
takeWhile v p x =
   takeWhilePrivate (p . (toAmplitudeScalar x v *>)) x



-- ToDo: this should be moved to a module that needs neither amplitude nor rate
spanPrimitive ::
   (SigG.Transform sig y, Amp.Primitive amp) =>
   (y -> Bool) ->
   (SigA.T rate amp (sig y) ->
    (SigA.T rate amp (sig y),
     SigA.T rate amp (sig y)))
spanPrimitive =
   spanPrivate

dropWhilePrimitive ::
   (SigG.Transform sig y, Amp.Primitive amp) =>
   (y -> Bool) ->
   SigA.T rate amp (sig y) ->
   SigA.T rate amp (sig y)
dropWhilePrimitive =
   dropWhilePrivate

takeWhilePrimitive ::
   (SigG.Transform sig y, Amp.Primitive amp) =>
   (y -> Bool) ->
   SigA.T rate amp (sig y) ->
   SigA.T rate amp (sig y)
takeWhilePrimitive =
   takeWhilePrivate



spanPrivate ::
   (SigG.Transform sig y) =>
   (y -> Bool) ->
   (SigA.T rate amp (sig y) ->
    (SigA.T rate amp (sig y),
     SigA.T rate amp (sig y)))
spanPrivate p x =
   let (y,z) = SigG.span p $ SigA.body x
   in  (SigA.replaceBody y x,
        SigA.replaceBody z x)

dropWhilePrivate ::
   (SigG.Transform sig y) =>
   (y -> Bool) ->
   SigA.T rate amp (sig y) ->
   SigA.T rate amp (sig y)
dropWhilePrivate p =
   SigA.processBody (SigG.dropWhile p)

takeWhilePrivate ::
   (SigG.Transform sig y) =>
   (y -> Bool) ->
   SigA.T rate amp (sig y) ->
   SigA.T rate amp (sig y)
takeWhilePrivate p =
   SigA.processBody (SigG.takeWhile p)



{-# INLINE leftFromStereo #-}
leftFromStereo :: (Dim.C u) =>
   SigA.R s u y (Stereo.T yv) -> SigA.R s u y yv
leftFromStereo = SigA.processBody (Sig.map Stereo.left)

{-# INLINE rightFromStereo #-}
rightFromStereo :: (Dim.C u) =>
   SigA.R s u y (Stereo.T yv) -> SigA.R s u y yv
rightFromStereo = SigA.processBody (Sig.map Stereo.right)



-- * glueing

type Signal s u y sig yv =
   SigA.T (Rate.Phantom s) (Amp.Dimensional u y) (sig yv)

{- |
Similar to @foldr1 append@ but more efficient and accurate,
because it reduces the number of amplifications.
Does not work for infinite lists,
because no maximum amplitude can be computed.
-}
{-# INLINE concat #-}
concat ::
   (Ord y, Field.C y, Dim.C u,
    Module.C y yv,
    SigG.Transform sig yv) =>
   [Signal s u y sig yv] -> Signal s u y sig yv
concat xs =
   concatVolume (List.maximum (List.map SigA.actualAmplitude xs)) xs

{- |
Give the output volume explicitly.
Does also work for infinite lists.
-}
{-# INLINE concatVolume #-}
concatVolume ::
   (Field.C y, Dim.C u,
    Module.C y yv,
    SigG.Transform sig yv) =>
   DN.T u y ->
   [Signal s u y sig yv] -> Signal s u y sig yv
concatVolume amp xs =
   let smps = List.map (SigA.vectorSamples (toAmplitudeScalar z)) xs
       z = SigA.fromBody amp (SigG.concat smps)
   in  z

{-# INLINE concatPrimitive #-}
concatPrimitive ::
   (CutG.Transform sig, Amp.Primitive amp) =>
   [SigA.T (Rate.Phantom s) amp sig] ->
   SigA.T (Rate.Phantom s) amp sig
concatPrimitive =
   SigA.primitiveFromBody . SigG.concat . List.map SigA.body


{-# INLINE merge #-}
merge ::
   (Ord y, Field.C y, Dim.C u,
    Module.C y yv0, Module.C y yv1,
    SigG.Transform sig0 yv0, SigG.Transform sig1 yv1) =>
   (sig0 yv0 -> sig1 yv1 -> sig2 yv2) ->
   Signal s u y sig0 yv0 -> Signal s u y sig1 yv1 -> Signal s u y sig2 yv2
merge f x0 x1 =
   mergeVolume f (max (SigA.actualAmplitude x0) (SigA.actualAmplitude x1)) x0 x1

{-# INLINE mergeVolume #-}
mergeVolume ::
   (Field.C y, Dim.C u,
    Module.C y yv0, Module.C y yv1,
    SigG.Transform sig0 yv0, SigG.Transform sig1 yv1) =>
   (sig0 yv0 -> sig1 yv1 -> sig2 yv2) ->
   DN.T u y ->
   Signal s u y sig0 yv0 -> Signal s u y sig1 yv1 -> Signal s u y sig2 yv2
mergeVolume f amp x y =
   let sampX = SigA.vectorSamples (toAmplitudeScalar z) x
       sampY = SigA.vectorSamples (toAmplitudeScalar z) y
       z = SigA.fromBody amp (f sampX sampY)
   in  z

{-# INLINE mergePrimitive #-}
mergePrimitive ::
   (Amp.Primitive amp) =>
   (sig0 -> sig1 -> sig2) ->
   SigA.T (Rate.Phantom s) amp sig0 ->
   SigA.T (Rate.Phantom s) amp sig1 ->
   SigA.T (Rate.Phantom s) amp sig2
mergePrimitive f x y =
   SigA.Cons Rate.Phantom Amp.primitive $
      f (SigA.body x) (SigA.body y)


{-# INLINE append #-}
append ::
   (Ord y, Field.C y, Dim.C u,
    Module.C y yv,
    SigG.Transform sig yv) =>
   Signal s u y sig yv -> Signal s u y sig yv -> Signal s u y sig yv
append = merge SigG.append

{-# INLINE appendVolume #-}
appendVolume ::
   (Field.C y, Dim.C u,
    Module.C y yv,
    SigG.Transform sig yv) =>
   DN.T u y ->
   Signal s u y sig yv -> Signal s u y sig yv -> Signal s u y sig yv
appendVolume = mergeVolume SigG.append

{-# INLINE appendPrimitive #-}
appendPrimitive ::
   (CutG.Transform sig, Amp.Primitive amp) =>
   SigA.T (Rate.Phantom s) amp sig ->
   SigA.T (Rate.Phantom s) amp sig ->
   SigA.T (Rate.Phantom s) amp sig
appendPrimitive = mergePrimitive SigG.append


{-# INLINE zip #-}
zip ::
   (Ord y, Field.C y, Dim.C u,
    Module.C y yv0, Module.C y yv1,
    SigG.Read sig yv0, SigG.Transform sig yv1, SigG.Transform sig (yv0,yv1)) =>
   Signal s u y sig yv0 -> Signal s u y sig yv1 -> Signal s u y sig (yv0,yv1)
zip =
   merge (SigG.zipWithState (,)) . SigA.restore

{-# INLINE zipVolume #-}
zipVolume ::
   (Field.C y, Dim.C u,
    Module.C y yv0, Module.C y yv1,
    SigG.Read sig yv0, SigG.Transform sig yv1, SigG.Transform sig (yv0,yv1)) =>
   DN.T u y ->
   Signal s u y sig yv0 -> Signal s u y sig yv1 -> Signal s u y sig (yv0,yv1)
zipVolume vol =
   mergeVolume (SigG.zipWithState (,)) vol . SigA.restore



{-# INLINE mergeStereo #-}
mergeStereo ::
   (Ord y, Field.C y, Dim.C u,
    Module.C y yv,
    SigG.Transform sig yv, SigG.Transform sig (Stereo.T yv)) =>
   Signal s u y sig yv -> Signal s u y sig yv -> Signal s u y sig (Stereo.T yv)
mergeStereo = merge (SigG.zipWith Stereo.cons)

{-# INLINE mergeStereoVolume #-}
mergeStereoVolume ::
   (Field.C y, Dim.C u,
    Module.C y yv,
    SigG.Transform sig yv, SigG.Transform sig (Stereo.T yv)) =>
   DN.T u y ->
   Signal s u y sig yv -> Signal s u y sig yv -> Signal s u y sig (Stereo.T yv)
mergeStereoVolume = mergeVolume (SigG.zipWith Stereo.cons)

{-# INLINE mergeStereoPrimitive #-}
mergeStereoPrimitive ::
   (Amp.Primitive amp, SigG.Transform sig y, SigG.Transform sig (Stereo.T y)) =>
   SigA.T (Rate.Phantom s) amp (sig y) ->
   SigA.T (Rate.Phantom s) amp (sig y) ->
   SigA.T (Rate.Phantom s) amp (sig (Stereo.T y))
mergeStereoPrimitive =
   mergePrimitive (SigG.zipWith Stereo.cons)



{-# INLINE zip3 #-}
zip3 ::
   (Ord y, Field.C y, Dim.C u,
    Module.C y yv0, Module.C y yv1, Module.C y yv2,
    SigG.Read sig yv0, SigG.Read sig yv1,
    SigG.Transform sig yv2, SigG.Transform sig (yv0, yv1, yv2)) =>
   Signal s u y sig yv0 -> Signal s u y sig yv1 -> Signal s u y sig yv2 ->
   Signal s u y sig (yv0,yv1,yv2)
zip3 x0 x1 x2 =
   zip3Volume
      (SigA.actualAmplitude x0 `max` SigA.actualAmplitude x1 `max` SigA.actualAmplitude x2)
      x0 x1 x2

{-# INLINE zip3Volume #-}
zip3Volume ::
   (Field.C y, Dim.C u,
    Module.C y yv0, Module.C y yv1, Module.C y yv2,
    SigG.Read sig yv0, SigG.Read sig yv1,
    SigG.Transform sig yv2, SigG.Transform sig (yv0, yv1, yv2)) =>
   DN.T u y ->
   Signal s u y sig yv0 -> Signal s u y sig yv1 -> Signal s u y sig yv2 ->
   Signal s u y sig (yv0,yv1,yv2)
zip3Volume amp x0 x1 x2 =
   let sampX0 = SigA.vectorSamples (toAmplitudeScalar z) (SigA.restore x0)
       sampX1 = SigA.vectorSamples (toAmplitudeScalar z) (SigA.restore x1)
       sampX2 = SigA.vectorSamples (toAmplitudeScalar z) x2
       z = SigA.fromBody amp (SigG.zipWithState3 (,,) sampX0 sampX1 sampX2)
   in  z


-- * miscellaneous

{-# INLINE selectBool #-}
selectBool ::
   (Ord y, Field.C y, Dim.C u,
    Module.C y yv,
    SigG.Read sig yv,
    SigG.Transform sig Bool, SigG.Transform sig yv) =>
   Signal s u y sig yv {- ^ False -} ->
   Signal s u y sig yv {- ^ True -} ->
   SigA.T (Rate.Phantom s) Amp.Abstract (sig Bool) ->
   Signal s u y sig yv
selectBool xf xt cs =
   SigA.processBody
      (flip (SigG.zipWithState (\(xfi,xti) c -> if c then xti else xfi))
          (SigA.body cs))
      (zip
         (SigA.restore xf)
         (SigA.restore xt))

{-# INLINE reverse #-}
reverse ::
   (SigG.Transform sig yv) =>
   SigA.T rate amp (sig yv) ->
   SigA.T rate amp (sig yv)
reverse =
   SigA.processBody SigG.reverse
