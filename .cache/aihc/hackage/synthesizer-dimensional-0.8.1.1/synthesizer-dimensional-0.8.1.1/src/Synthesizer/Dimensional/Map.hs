{- |
Maps that handle pairs of amplitudes and sampled values.
They are a special form of arrows.
-}
module Synthesizer.Dimensional.Map where

import qualified Synthesizer.Dimensional.Sample as Sample

import qualified Synthesizer.Dimensional.Arrow as ArrowD

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
import qualified Synthesizer.Dimensional.Amplitude as Amp

import Control.Arrow (Arrow, )
import Control.Category (Category, )

import qualified Synthesizer.Generic.Signal as SigG

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import qualified Algebra.Module as Module
import qualified Algebra.Field  as Field

import qualified Data.Function as Func
import qualified Data.Tuple as Tuple
import Data.Tuple.HT as TupleHT (swap, )

import Prelude hiding (map, fst, snd, id, )



{- |
This type shall ensure, that you do not accidentally
bring amplitudes and the corresponding low-level signal values out of sync.
We also use it for generation of internal control parameters
in "Synthesizer.Dimensional.Causal.ControlledProcess".
In principle this could also be 'Causal.T',
but maps are not bound to a sampling rate,
and thus do not need the @s@ type parameter.
-}
type T = ArrowD.T (->)

type Single amp0 amp1 yv0 yv1 =
        ArrowD.Single (->) amp0 amp1 yv0 yv1


consFlip ::
   (Sample.Amplitude sample0 ->
    (Sample.Amplitude sample1,
     Sample.Displacement sample0 ->
     Sample.Displacement sample1)) ->
   T sample0 sample1
consFlip f =
   ArrowD.Cons $ TupleHT.swap . f


{-# INLINE apply #-}
apply ::
   (SigG.Transform sig yv0, SigG.Transform sig yv1) =>
   Single amp0 amp1 yv0 yv1 ->
   SigA.T rate amp0 (sig yv0) ->
   SigA.T rate amp1 (sig yv1)
apply = ArrowD.apply

{-# INLINE applyFlat #-}
applyFlat ::
   (Flat.C yv0 amp0,
    SigG.Transform sig yv0, SigG.Transform sig yv1) =>
   Single (Amp.Flat yv0) amp1 yv0 yv1 ->
   SigA.T rate amp0 (sig yv0) ->
   SigA.T rate amp1 (sig yv1)
applyFlat = ArrowD.applyFlat


{-# INLINE forceDimensionalAmplitude #-}
forceDimensionalAmplitude ::
   (Dim.C v, Field.C y, Module.C y yv, Arrow arrow) =>
   DN.T v y ->
   ArrowD.Single arrow (Amp.Dimensional v y) (Amp.Dimensional v y) yv yv
forceDimensionalAmplitude =
   ArrowD.forceDimensionalAmplitude

{-# INLINE forcePrimitiveAmplitude #-}
forcePrimitiveAmplitude ::
   (Amp.Primitive amp, Arrow arrow) =>
   ArrowD.Single arrow amp amp yv yv
forcePrimitiveAmplitude =
   independent (const Amp.primitive) Func.id


{- |
We restrict the amplitude types to those of class 'Amplitude'.
Otherwise 'mapAmplitude' could be abused
for bringing amplitudes and respective sample values out of sync.
For mapping amplitudes that are nested in some pairs,
use it in combination with 'first' and 'second'.

FIXME:
This function however still breaks the abstraction,
since normally it should not be observable
how the volume is balanced between amplitude and signal.
This function allows to replace an actual amplitude by 'Flat',
which also breaks the abstraction.
This may only be used for proportional mappings.
See 'SigA.T'.
-}
{-# INLINE mapAmplitude #-}
mapAmplitude ::
   (Amp.C amp0, Amp.C amp1, Arrow arrow) =>
   (amp0 -> amp1) ->
   ArrowD.Single arrow amp0 amp1 yv yv
mapAmplitude f =
   independent f Func.id

{- |
FIXME: This function is unsafe.
Only use it for proportional mappings.
See 'SigA.T'.
-}
{-# INLINE mapAmplitudeSameType #-}
mapAmplitudeSameType ::
   (Arrow arrow) =>
   (Sample.Amplitude sample -> Sample.Amplitude sample) ->
   ArrowD.T arrow sample sample
mapAmplitudeSameType f =
   independent f Func.id


{- |
This function can be abused to bring the amplitudes out of order.
So be careful!
-}
{-# INLINE independent #-}
independent ::
   (Arrow arrow) =>
   (Sample.Amplitude sample0 -> Sample.Amplitude sample1) ->
   (Sample.Displacement sample0 -> Sample.Displacement sample1) ->
   ArrowD.T arrow sample0 sample1
independent =
   ArrowD.independentMap

{-# INLINE id #-}
id ::
   (Category arrow) =>
   ArrowD.T arrow sample sample
id = ArrowD.id

{-# INLINE double #-}
double ::
   (Arrow arrow) =>
   ArrowD.T arrow sample (sample, sample)
double =
   ArrowD.double

{-# INLINE fst #-}
fst ::
   (Arrow arrow) =>
   ArrowD.T arrow (sample0,sample1) sample0
fst =
   let aux = Tuple.fst
   in  independent aux aux

{-# INLINE snd #-}
snd ::
   (Arrow arrow) =>
   ArrowD.T arrow (sample0,sample1) sample1
snd =
   let aux = Tuple.snd
   in  independent aux aux

{-# INLINE swap #-}
swap ::
   (Arrow arrow) =>
   ArrowD.T arrow (sample0,sample1) (sample1,sample0)
swap =
   let aux = TupleHT.swap
   in  independent aux aux

{-# INLINE balanceRight #-}
balanceRight ::
   (Arrow arrow) =>
   ArrowD.T arrow
      ((sample0,sample1), sample2) (sample0, (sample1,sample2))
balanceRight =
   let aux = \((a,b), c) -> (a, (b,c))
   in  independent aux aux

{-# INLINE balanceLeft #-}
balanceLeft ::
   (Arrow arrow) =>
   ArrowD.T arrow
      (sample0, (sample1,sample2)) ((sample0,sample1), sample2)
balanceLeft =
   let aux = \(a, (b,c)) -> ((a,b), c)
   in  independent aux aux

{-# INLINE packTriple #-}
packTriple ::
   (Arrow arrow) =>
   ArrowD.T arrow
      (sample0,(sample1,sample2)) (sample0,sample1,sample2)
packTriple =
   let aux = \(a,(b,c)) -> (a,b,c)
   in  independent aux aux

{-# INLINE unpackTriple #-}
unpackTriple ::
   (Arrow arrow) =>
   ArrowD.T arrow
      (sample0,sample1,sample2) (sample0,(sample1,sample2))
unpackTriple =
   let aux = \(a,b,c) -> (a,(b,c))
   in  independent aux aux
