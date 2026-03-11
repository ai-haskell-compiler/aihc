{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.State.Filter.NonRecursive (
   amplify,
   amplifyVector,
   envelope,
   envelopeVector,
   fadeInOut,
   generic,
   binomial,
   binomial1,
   sums,
   inverseFrequencyModulationFloor,
   inverseFrequencyModulationCeiling,
   differentiate,
   differentiateCenter,
   differentiate2,
   ) where

import Synthesizer.Basic.Filter.NonRecursive (ratioFreqToVariance, )

import qualified Synthesizer.State.Signal as Sig

import qualified Synthesizer.State.Filter.Delay as Delay
import qualified Synthesizer.State.Control as Ctrl

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import Data.Function.HT (nest, )
import Data.Tuple.HT (mapFst, )

import NumericPrelude.Numeric
import NumericPrelude.Base



{- * Envelope application -}

{-# INLINE amplify #-}
amplify :: (Ring.C a) => a -> Sig.T a -> Sig.T a
amplify v = Sig.map (v*)

{-# INLINE amplifyVector #-}
amplifyVector :: (Module.C a v) => a -> Sig.T v -> Sig.T v
amplifyVector v = Sig.map (v*>)


{-# INLINE envelope #-}
envelope :: (Ring.C a) =>
      Sig.T a  {-^ the envelope -}
   -> Sig.T a  {-^ the signal to be enveloped -}
   -> Sig.T a
envelope = Sig.zipWith (*)

{-# INLINE envelopeVector #-}
envelopeVector :: (Module.C a v) =>
      Sig.T a  {-^ the envelope -}
   -> Sig.T v  {-^ the signal to be enveloped -}
   -> Sig.T v
envelopeVector = Sig.zipWith (*>)



{-# INLINE fadeInOut #-}
fadeInOut :: (Field.C a) =>
   Int -> Int -> Int -> Sig.T a -> Sig.T a
fadeInOut tIn tHold tOut =
   let leadIn  = Sig.take tIn  $ Ctrl.linear (  recip (fromIntegral tIn))  zero
       leadOut = Sig.take tOut $ Ctrl.linear (- recip (fromIntegral tOut)) one
       hold    = Sig.replicate tHold one
   in  envelope (leadIn `Sig.append` hold `Sig.append` leadOut)


{- * Smoothing -}


{-| Unmodulated non-recursive filter -}
{-# INLINE generic #-}
generic :: (Module.C a v) =>
   Sig.T a -> Sig.T v -> Sig.T v
generic m x =
   let mr = Sig.reverse m
       xp = Delay.staticPos (pred (Sig.length m)) x
   in  Sig.mapTails (Sig.linearComb mr) xp

{-
genericSlow :: Module.C a v =>
   Sig.T a -> Sig.T v -> Sig.T v
genericSlow m x =
   let mr = Sig.reverse m
       xp = delay (pred (Sig.length m)) x
   in  Sig.fromList (map (Sig.linearComb mr) (init (Sig.tails xp)))
-}

{-
{- |
@eps@ is the threshold relatively to the maximum.
That is, if the gaussian falls below @eps * gaussian 0@,
then the function truncated.
-}
gaussian ::
   (Trans.C a, RealField.C a, Module.C a v) =>
   a -> a -> a -> Sig.T v -> Sig.T v
gaussian eps ratio freq =
   let var    = ratioFreqToVariance ratio freq
       area   = var * sqrt (2*pi)
       gau t  = exp (-(t/var)^2/2) / area
       width  = ceiling (var * sqrt (-2 * log eps))  -- inverse gau
       gauSmp = map (gau . fromIntegral) [-width .. width]
   in  drop width . generic gauSmp
-}

{-
GNUPlot.plotList [] (take 1000 $ gaussian 0.001 0.5 0.04 (Filter.Test.chirp 5000) :: [Double])

The filtered chirp must have amplitude 0.5 at 400 (0.04*10000).
-}

{-
  We want to approximate a Gaussian by a binomial filter.
  The latter one can be implemented by a convolutional power.
  However we still require a number of operations per sample
  which is proportional to the variance.
-}
{-# INLINE binomial #-}
binomial ::
   (Trans.C a, RealField.C a, Module.C a v) =>
   a -> a -> Sig.T v -> Sig.T v
binomial ratio freq =
   let width = ceiling (2 * ratioFreqToVariance ratio freq ^ 2)
   in  Sig.drop width . nest (2*width) ((asTypeOf 0.5 freq *>) . binomial1)

{-# INLINE binomial1 #-}
binomial1 :: (Additive.C v) => Sig.T v -> Sig.T v
binomial1 = Sig.mapAdjacent (+)





{- |
Moving (uniformly weighted) average in the most trivial form.
This is very slow and needs about @n * length x@ operations.
-}
{-# INLINE sums #-}
sums :: (Additive.C v) => Int -> Sig.T v -> Sig.T v
sums n = Sig.mapTails (Sig.sum . Sig.take n)


{-
sumsDownsample2 :: (Additive.C v) => Sig.T v -> Sig.T v
sumsDownsample2 (x0:x1:xs) = (x0+x1) : sumsDownsample2 xs
sumsDownsample2 xs         = xs

downsample2 :: Sig.T a -> Sig.T a
downsample2 (x0:_:xs) = x0 : downsample2 xs
downsample2 xs        = xs


{- |
Given a list of numbers
and a list of sums of (2*k) of successive summands,
compute a list of the sums of (2*k+1) or (2*k+2) summands.

Eample for 2*k+1

@
 [0+1+2+3, 2+3+4+5, 4+5+6+7, ...] ->
    [0+1+2+3+4, 1+2+3+4+5, 2+3+4+5+6, 3+4+5+6+7, 4+5+6+7+8, ...]
@

Example for 2*k+2

@
 [0+1+2+3, 2+3+4+5, 4+5+6+7, ...] ->
    [0+1+2+3+4+5, 1+2+3+4+5+6, 2+3+4+5+6+7, 3+4+5+6+7+8, 4+5+6+7+8+9, ...]
@
-}
sumsUpsampleOdd :: (Additive.C v) => Int -> Sig.T v -> Sig.T v -> Sig.T v
sumsUpsampleOdd n {- 2*k -} xs ss =
   let xs2k = drop n xs
   in  (head ss + head xs2k) :
          concat (zipWith3 (\s x0 x2k -> [x0+s, s+x2k])
                           (tail ss)
                           (downsample2 (tail xs))
                           (tail (downsample2 xs2k)))

sumsUpsampleEven :: (Additive.C v) => Int -> Sig.T v -> Sig.T v -> Sig.T v
sumsUpsampleEven n {- 2*k -} xs ss =
   sumsUpsampleOdd (n+1) xs (zipWith (+) ss (downsample2 (drop n xs)))

sumsPyramid :: (Additive.C v) => Int -> Sig.T v -> Sig.T v
sumsPyramid n xs =
   let aux 1 ys = ys
       aux 2 ys = ys + tail ys
       aux m ys =
          let ysd = sumsDownsample2 ys
          in  if even m
                then sumsUpsampleEven (m-2) ys (aux (div (m-2) 2) ysd)
                else sumsUpsampleOdd  (m-1) ys (aux (div (m-1) 2) ysd)
   in  aux n xs


propSums :: Bool
propSums =
   let n  = 1000
       xs = [0::Double ..]
       naive   =              sums        n xs
       rec     = drop (n-1) $ sumsRec     n xs
       pyramid =              sumsPyramid n xs
   in  and $ take 1000 $
         zipWith3 (\x y z -> x==y && y==z) naive rec pyramid

-}


{- |
This is inverse to frequency modulation.
If all control values in @ctrl@ are above one, then it holds:
@ frequencyModulation ctrl (inverseFrequencyModulationFloor ctrl xs) == xs @.
Otherwise 'inverseFrequencyModulationFloor' is lossy.
For the precise property
we refer to "Test.Sound.Synthesizer.Plain.Interpolation".
The modulation uses constant interpolation.
Other interpolation types are tricky to implement,
since they would need interpolation on non-equidistant grids.
Ok, at least linear interpolation could be supported
with acceptable effort,
but perfect reconstruction would be hard to get.
The process is not causal in any of its inputs,
however control and input are aligned.

If you use interpolation for resampling or frequency modulation,
you may want to smooth the signal before resampling
according to the local resampling factor.
However you cannot simply use the resampling control
to also control the smoothing,
because of the subsequent distortion by resampling.
Instead you have to stretch the control inversely to the resampling factor.
This is the task of this function.
It may be applied like:

> frequencyModulation ctrl (smooth (inverseFrequencyModulationFloor ctrl ctrl) xs)
-}
{-# INLINE inverseFrequencyModulationFloor #-}
inverseFrequencyModulationFloor ::
   (Ord t, Ring.C t) =>
   Sig.T t -> Sig.T v -> Sig.T v
inverseFrequencyModulationFloor =
   inverseFrequencyModulationGen (<1)

{-
Also a sensible implementation,
but incompatible with relative interpolation / frequency modulation.
-}
{-# INLINE inverseFrequencyModulationCeiling #-}
inverseFrequencyModulationCeiling ::
   (Ord t, Ring.C t) =>
   Sig.T t -> Sig.T v -> Sig.T v
inverseFrequencyModulationCeiling =
   inverseFrequencyModulationGen (<=0)


{-# INLINE inverseFrequencyModulationGen #-}
inverseFrequencyModulationGen ::
   (Ord t, Ring.C t) =>
   (t -> Bool) ->
   Sig.T t -> Sig.T v -> Sig.T v
inverseFrequencyModulationGen p ctrl xs =
   Sig.runSwitchL
      (Sig.zip ctrl xs)
      (\switch ->
         switch Sig.empty
            (curry $
             Sig.unfoldR
                (let go (c,x) cxs =
                        if p c
                          then switch Nothing (go . mapFst (c+)) cxs
                          else Just (x, ((c-1,x),cxs))
                 in  uncurry go)))


{- * Filter operators from calculus -}

{- |
Forward difference quotient.
Shortens the signal by one.
Inverts 'Synthesizer.State.Filter.Recursive.Integration.run' in the sense that
@differentiate (zero : integrate x) == x@.
The signal is shifted by a half time unit.
-}
{-# INLINE differentiate #-}
differentiate :: Additive.C v => Sig.T v -> Sig.T v
differentiate x = Sig.mapAdjacent subtract x

{- |
Central difference quotient.
Shortens the signal by two elements,
and shifts the signal by one element.
(Which can be fixed by prepending an appropriate value.)
For linear functions this will yield
essentially the same result as 'differentiate'.
You obtain the result of 'differentiateCenter'
if you smooth the one of 'differentiate'
by averaging pairs of adjacent values.

ToDo: Vector variant
-}
{- We use mapAdjacent in order to avoid recomputation of the input signal -}
{-# INLINE differentiateCenter #-}
differentiateCenter :: Field.C v => Sig.T v -> Sig.T v
differentiateCenter =
   Sig.mapAdjacent (\(x0,_) (_,x1) -> (x1 - x0) * (1/2)) .
   Sig.mapAdjacent (,)
{-
differentiateCenter :: Field.C v => Sig.T v -> Sig.T v
differentiateCenter x =
   Sig.map ((1/2)*) $
   Sig.zipWith subtract x (Sig.tail (Sig.tail x))
-}

{- |
Second derivative.
It is @differentiate2 == differentiate . differentiate@
but 'differentiate2' should be faster.
-}
{-# INLINE differentiate2 #-}
differentiate2 :: Additive.C v => Sig.T v -> Sig.T v
differentiate2 = differentiate . differentiate
{-
differentiate2 :: Additive.C v => Sig.T v -> Sig.T v
differentiate2 xs0 =
   let xs1 = Sig.tail xs0
       xs2 = Sig.tail xs1
   in  Sig.zipWith3 (\x0 x1 x2 -> x0+x2-(x1+x1)) xs0 xs1 xs2
-}
