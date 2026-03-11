{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{- |
A wrapper around @(->)@ or @Causal.Process@
that adds amplitude handling to the Arrow paradigm.
This wrapper unifies "Synthesizer.Dimensional.Map"
and "Synthesizer.Dimensional.Causal.Process".
-}
module Synthesizer.Dimensional.Arrow where

import qualified Synthesizer.Dimensional.Sample as Sample
import Synthesizer.Dimensional.Sample (Amplitude, Displacement, )

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.Causal.Arrow as CausalArrow

import qualified Control.Arrow as Arrow
import qualified Control.Category as Category
import Control.Arrow (Arrow, ArrowLoop, (>>>), (***), )
import Control.Category (Category, )

import Control.Applicative (Applicative, liftA2, )

import qualified Synthesizer.State.Signal as Sig
import qualified Synthesizer.Generic.Signal as SigG

import qualified Algebra.Module as Module
import qualified Algebra.Field  as Field
import qualified Algebra.Ring   as Ring
import Algebra.Module ((*>))

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import NumericPrelude.Numeric (one)
import NumericPrelude.Base hiding (id)
import Prelude ()



{- |
The sample type parameters
can be arbitrarily nested tuples of 'Samples'.
Type functions are used for untangling amplitudes and displacements.
We use this approach in order to be able to match
(as good as possible) the Arrow type class.
-}
newtype T arrow sample0 sample1 =
   Cons (Amplitude sample0 ->
            (arrow (Displacement sample0) (Displacement sample1),
             Amplitude sample1))

type Single arrow amp0 amp1 yv0 yv1 =
        T arrow (Sample.T amp0 yv0) (Sample.T amp1 yv1)


{-
It is tempting to declare a rate parameter for the process type,
instead of putting the rate phantom into the arrow.
However, Map would then be defined as

> type Map amp0 amp1 yv0 yv1 = T (forall rate. rate) amp0 amp1 (yv0->yv1)@

which is at least ugly. Even more, in module Rate we would need

> class Applicable process signal | signal -> process
> instance Applicable (Phantom s) (Phantom s)
> instance Applicable (forall process. process) (Actual rate)

and this is not possible, at all.

With the current approach we can have
both generic apply functions and generic arrow combinators.
-}

class CausalArrow.C arrow => Applicable arrow rate

instance Applicable (->) rate




infixl 9 `apply`

-- we need this generality in ControlledProcess.applyConverter
{-# INLINE apply #-}
apply ::
   (SigG.Transform sig (Displacement sample0),
    SigG.Transform sig (Displacement sample1),
    Applicable arrow rate) =>
   T arrow sample0 sample1 ->
   SigA.T rate (Amplitude sample0) (sig (Displacement sample0)) ->
   SigA.T rate (Amplitude sample1) (sig (Displacement sample1))
{-
   (SigG.Transform sig yv0, SigG.Transform sig yv1, Applicable arrow rate) =>
   Single arrow amp0 amp1 yv0 yv1 ->
   SigA.T rate amp0 (sig yv0) ->
   SigA.T rate amp1 (sig yv1)
-}
apply (Cons f) (SigA.Cons rate xAmp samples) =
   let (arrow, yAmp) = f xAmp
   in  SigA.Cons rate yAmp (CausalArrow.apply arrow samples)

{-# INLINE applyFlat #-}
applyFlat ::
   (Flat.C yv0 amp0,
    SigG.Transform sig yv0,
    SigG.Transform sig yv1, Applicable arrow rate) =>
   Single arrow (Amp.Flat yv0) amp1 yv0 yv1 ->
   SigA.T rate amp0 (sig yv0) ->
   SigA.T rate amp1 (sig yv1)
applyFlat f =
   apply (canonicalizeFlat >>> f)

{-# INLINE canonicalizeFlat #-}
canonicalizeFlat ::
   (Flat.C y flat, Arrow arrow) =>
   Single arrow flat (Amp.Flat y) y y
canonicalizeFlat =
   Cons $ \ amp -> (Arrow.arr (Flat.amplifySample amp), Amp.Flat)


{-# INLINE applyConst #-}
applyConst ::
   (Amp.C amp1, Ring.C y0, CausalArrow.C arrow) =>
   Single arrow (Amp.Numeric amp0) amp1 y0 yv1 ->
   amp0 ->
   SigA.T (Rate.Phantom s) amp1 (Sig.T yv1)
applyConst (Cons f) x =
   let (arrow, yAmp) = f (Amp.Numeric x)
   in  SigA.Cons Rate.Phantom yAmp
          (CausalArrow.apply arrow (Sig.repeat one))


infixl 0 $/:, $/-

{-# INLINE ($/:) #-}
($/:) ::
   (Applicative f,
    SigG.Transform sig yv0, SigG.Transform sig yv1,
    Applicable arrow rate) =>
   f (Single arrow amp0 amp1 yv0 yv1) ->
   f (SigA.T rate amp0 (sig yv0)) ->
   f (SigA.T rate amp1 (sig yv1))
($/:) = liftA2 apply

{-# INLINE ($/-) #-}
($/-) ::
   (Amp.C amp1, Functor f, Ring.C y0, CausalArrow.C arrow) =>
   f (Single arrow (Amp.Numeric amp0) amp1 y0 yv1) ->
   amp0 ->
   f (SigA.T (Rate.Phantom s) amp1 (Sig.T yv1))
($/-) p x = fmap (flip applyConst x) p



{-# INLINE id #-}
id ::
   (Category arrow) =>
   T arrow sample sample
id =
   Cons (\amp -> (Category.id, amp))


{-# INLINE compose #-}
compose ::
   (Category arrow) =>
   T arrow sample0 sample1 ->
   T arrow sample1 sample2 ->
   T arrow sample0 sample2
compose (Cons f) (Cons g) =
   Cons $ \ xAmp ->
      let (causalXY, yAmp) = f xAmp
          (causalYZ, zAmp) = g yAmp
      in  (causalXY Arrow.>>> causalYZ, zAmp)


instance (Category arrow) => Category (T arrow) where
   {-# INLINE id #-}
   id = id
   {-# INLINE (.) #-}
   (.) = flip compose


{- |
This instance lacks an implementation for 'arr'.
However the syntactic sugar for arrows
uses 'arr' for shuffling the operands.
Actually shuffling is possible for our arrow,
but lifting general functions is a problem.
If you want to use arrow syntax,
you should hide the 'arr' from Control.Arrow
and use the one provided as plain function, here.
-}
instance (Arrow arrow) => Arrow (T arrow) where
   {-# INLINE first #-}
   {-# INLINE second #-}
   {-# INLINE (***) #-}
   {-# INLINE (&&&) #-}

   arr = error "Dimensional.Arrow.arr: sorry, there is no reasonable implementation"
   first  = first
   second = second
   (***)  = split
   (&&&)  = fanout


{- |
This implementation would work for all 'f's
where the output amplitude does not depend on the input displacement.
This is true for all shuffling operations
that are needed in the translation of the arrow syntax.
However, for the implementation we would need type constraints
of the function passed to 'arr'
and this is not allowed.
-}
{-# INLINE arr #-}
arr ::
   (Arrow arrow, Sample.Build sample0, Sample.Inspect sample1) =>
   (sample0 -> sample1) -> T arrow sample0 sample1
arr f = Cons $ \amp0 ->
   (Arrow.arr $ \yv0 ->
       Sample.displacement $ f $ Sample.build amp0 yv0,
    Sample.amplitude $ f $ Sample.build amp0 $
       error $ "Dimensional.Arrow.arr: " ++
               "output amplitude must not depend on input displacement")

{-# INLINE first #-}
first ::
   (Arrow arrow) =>
   T arrow sample0 sample1 ->
   T arrow (sample0, sample) (sample1, sample)
first (Cons f) =
   Cons $ \ (xAmp, amp) ->
      let (arrow, yAmp) = f xAmp
      in  (Arrow.first arrow, (yAmp, amp))

{-# INLINE second #-}
second ::
   (Arrow arrow) =>
   T arrow sample0 sample1 ->
   T arrow (sample, sample0) (sample, sample1)
second (Cons f) =
   Cons $ \ (amp, xAmp) ->
      let (arrow, yAmp) = f xAmp
      in  (Arrow.second arrow, (amp, yAmp))

{-# INLINE split #-}
split ::
   (Arrow arrow) =>
   T arrow sample0 sample1 ->
   T arrow sample2 sample3 ->
   T arrow (sample0, sample2) (sample1, sample3)
split f g =
   compose (first f) (second g)

{-# INLINE fanout #-}
fanout ::
   (Arrow arrow) =>
   T arrow sample sample0 ->
   T arrow sample sample1 ->
   T arrow sample (sample0, sample1)
fanout f g =
   compose double (split f g)




-- * map functions

{-
This has become a bit safer by the use of type families,
since now we can assert that amplitude and displacement tuples match.
Unless someone adds inappropriate type instances.
-}
independentMap ::
   (Arrow arrow) =>
   (Amplitude sample0 -> Amplitude sample1) ->
   (Displacement sample0 -> Displacement sample1) ->
   T arrow sample0 sample1
independentMap f g =
   Cons (\amp -> (Arrow.arr g, f amp))

double ::
   (Arrow arrow) =>
   T arrow sample (sample, sample)
double =
   let aux :: sample -> (sample, sample)
       aux x = (x, x)
   in  independentMap aux aux

{-# INLINE forceDimensionalAmplitude #-}
forceDimensionalAmplitude ::
   (Dim.C v, Field.C y, Module.C y yv, Arrow arrow) =>
   DN.T v y ->
   Single arrow (Amp.Dimensional v y) (Amp.Dimensional v y) yv yv
forceDimensionalAmplitude ampOut =
   Cons $ \(Amp.Numeric ampIn) ->
      (Arrow.arr (DN.divToScalar ampIn ampOut *>),
       Amp.Numeric ampOut)



{- |
I will call the connection from input to output amplitudes of type @amp@
the looping channel.
It is essential, that the looping channel decouples output from input amplitude.
You can achieve this by inserting one of the @forceAmplitude@ functions
somewhere in the looping channel.
-}
{-# INLINE loop #-}
loop ::
   (ArrowLoop arrow) =>
   T arrow (restSampleIn, sample) (restSampleOut, sample) ->
   T arrow restSampleIn restSampleOut
loop (Cons f) =
   Cons $ \restAmpIn ->
      let (arrow, (restAmpOut, amp)) = f (restAmpIn, amp)
      in  (Arrow.loop arrow, restAmpOut)


{-# INLINE loopVolume #-}
loopVolume ::
   (Field.C y, Module.C y yv, Dim.C v,
    ArrowLoop arrow) =>
   DN.T v y ->
   T arrow
     (restSampleIn,  Sample.T (Amp.Dimensional v y) yv)
     (restSampleOut, Sample.T (Amp.Dimensional v y) yv) ->
   T arrow restSampleIn restSampleOut
loopVolume ampIn f =
   loop (f >>> second (forceDimensionalAmplitude ampIn))


{-# INLINE loop2Volume #-}
loop2Volume ::
   (Field.C y0, Module.C y0 yv0, Dim.C v0,
    Field.C y1, Module.C y1 yv1, Dim.C v1,
    ArrowLoop arrow) =>
   (DN.T v0 y0, DN.T v1 y1) ->
   T arrow
     (restSampleIn,  (Sample.T (Amp.Dimensional v0 y0) yv0,
                      Sample.T (Amp.Dimensional v1 y1) yv1))
     (restSampleOut, (Sample.T (Amp.Dimensional v0 y0) yv0,
                      Sample.T (Amp.Dimensional v1 y1) yv1)) ->
   T arrow restSampleIn restSampleOut
loop2Volume (ampIn0,ampIn1) f =
   loop (f >>> second
      (forceDimensionalAmplitude ampIn0 ***
       forceDimensionalAmplitude ampIn1))
