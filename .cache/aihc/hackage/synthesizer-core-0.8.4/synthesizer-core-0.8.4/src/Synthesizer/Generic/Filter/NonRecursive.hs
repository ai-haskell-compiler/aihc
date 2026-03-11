{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2011
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Generic.Filter.NonRecursive (
   negate,
   amplify,
   amplifyVector,
   normalize,
   envelope,
   envelopeVector,
   fadeInOut,
   delay,
   delayPad,
   delayPos,
   delayNeg,
   delayLazySize,
   delayPadLazySize,
   delayPosLazySize,
   binomialMask,
   binomial,
   binomial1,
   sums,
   sumsDownsample2,
   downsample2,
   downsample,
   sumRange,
   pyramid,
   sumRangeFromPyramid,
   sumsPosModulated,
   sumsPosModulatedPyramid,
   movingAverageModulatedPyramid,
   inverseFrequencyModulationFloor,

   differentiate,
   differentiateCenter,
   differentiate2,

   generic,
   karatsubaFinite,
   karatsubaFiniteInfinite,
   karatsubaInfinite,

   Pair,
   convolvePair,
   sumAndConvolvePair,
   Triple,
   convolvePairTriple,
   convolveTriple,
   sumAndConvolveTriple,
   sumAndConvolveTripleAlt,
   Quadruple,
   convolveQuadruple,
   sumAndConvolveQuadruple,
   sumAndConvolveQuadrupleAlt,

   -- for use in Storable.Filter.NonRecursive
   maybeAccumulateRangeFromPyramid,
   accumulatePosModulatedFromPyramid,
   withPaddedInput,
   -- for use in Generic.Fourier
   addShiftedSimple,

   -- for testing
   consumeRangeFromPyramid,
   sumRangeFromPyramidReverse,
   sumRangeFromPyramidFoldr,
   ) where

import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Generic.Control as Ctrl
import qualified Synthesizer.Generic.LengthSignal as SigL

import qualified Synthesizer.Basic.Filter.NonRecursive as Filt
import qualified Synthesizer.State.Filter.NonRecursive as FiltS
import qualified Synthesizer.State.Signal as SigS

import Control.Monad (mplus, )
import Data.Function.HT (nest, )
import Data.Tuple.HT (mapSnd, mapPair, )
import Data.Maybe.HT (toMaybe, )

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import qualified NumericPrelude.Numeric as NP
import NumericPrelude.Numeric hiding (negate, )
import NumericPrelude.Base


-- * Envelope application

{-# INLINE negate #-}
negate ::
   (Additive.C a, SigG.Transform sig a) =>
   sig a -> sig a
negate = SigG.map Additive.negate

{-# INLINE amplify #-}
amplify ::
   (Ring.C a, SigG.Transform sig a) =>
   a -> sig a -> sig a
amplify v = SigG.map (v*)

{-# INLINE amplifyVector #-}
amplifyVector ::
   (Module.C a v, SigG.Transform sig v) =>
   a -> sig v -> sig v
amplifyVector v = SigG.map (v*>)

{-# INLINE normalize #-}
normalize ::
   (Field.C a, SigG.Transform sig a) =>
   (sig a -> a) -> sig a -> sig a
normalize volume xs =
   amplify (recip $ volume xs) xs

{-# INLINE envelope #-}
envelope ::
   (Ring.C a, SigG.Transform sig a) =>
      sig a  {-^ the envelope -}
   -> sig a  {-^ the signal to be enveloped -}
   -> sig a
envelope = SigG.zipWith (*)

{-# INLINE envelopeVector #-}
envelopeVector ::
   (Module.C a v, SigG.Read sig a, SigG.Transform sig v) =>
      sig a  {-^ the envelope -}
   -> sig v  {-^ the signal to be enveloped -}
   -> sig v
envelopeVector = SigG.zipWith (*>)



{-# INLINE fadeInOut #-}
fadeInOut ::
   (Field.C a, SigG.Write sig a) =>
   Int -> Int -> Int -> sig a -> sig a
fadeInOut tIn tHold tOut xs =
   let slopeIn  =                  recip (fromIntegral tIn)
       slopeOut = Additive.negate (recip (fromIntegral tOut))
       {-
       Since we use the size only for the internal envelope
       no laziness effect can be observed outside the function.
       We could also create the envelope as State.Signal.
       But I assume that concatenating chunks of an envelope
       is more efficient than concatenating generator loops.
       However, our intermediate envelope is still observable,
       because we have to use SigG.Write class.
       -}
       leadIn  = SigG.take tIn  $ Ctrl.linear SigG.defaultLazySize slopeIn  0
       leadOut = SigG.take tOut $ Ctrl.linear SigG.defaultLazySize slopeOut 1
       (partIn, partHoldOut) = SigG.splitAt tIn xs
       (partHold, partOut)   = SigG.splitAt tHold partHoldOut
   in  envelope leadIn partIn `SigG.append`
       partHold `SigG.append`
       envelope leadOut partOut


-- * Delay

{-# INLINE delay #-}
delay :: (Additive.C y, SigG.Write sig y) =>
   Int -> sig y -> sig y
delay =
   delayPad zero

{-# INLINE delayPad #-}
delayPad :: (SigG.Write sig y) =>
   y -> Int -> sig y -> sig y
delayPad z n =
   if n<0
     then SigG.drop (Additive.negate n)
     else SigG.append (SigG.replicate SigG.defaultLazySize n z)

{-# INLINE delayPos #-}
delayPos :: (Additive.C y, SigG.Write sig y) =>
   Int -> sig y -> sig y
delayPos n =
   SigG.append (SigG.replicate SigG.defaultLazySize n zero)

{-# INLINE delayNeg #-}
delayNeg :: (SigG.Transform sig y) =>
   Int -> sig y -> sig y
delayNeg = SigG.drop



{-# INLINE delayLazySize #-}
delayLazySize :: (Additive.C y, SigG.Write sig y) =>
   SigG.LazySize -> Int -> sig y -> sig y
delayLazySize size =
   delayPadLazySize size zero

{- |
The pad value @y@ must be defined,
otherwise the chunk size of the padding can be observed.
-}
{-# INLINE delayPadLazySize #-}
delayPadLazySize :: (SigG.Write sig y) =>
   SigG.LazySize -> y -> Int -> sig y -> sig y
delayPadLazySize size z n =
   if n<0
     then SigG.drop (Additive.negate n)
     else SigG.append (SigG.replicate size n z)

{-# INLINE delayPosLazySize #-}
delayPosLazySize :: (Additive.C y, SigG.Write sig y) =>
   SigG.LazySize -> Int -> sig y -> sig y
delayPosLazySize size n =
   SigG.append (SigG.replicate size n zero)


-- * smoothing

binomialMask ::
   (Field.C a, SigG.Write sig a) =>
   SigG.LazySize ->
   Int -> sig a
binomialMask size n =
   SigG.unfoldR size
      (\(x, a, b) ->
          toMaybe (b>=0)
             (x, (x * fromInteger b / fromInteger (a+1), a+1, b-1)))
      (recip $ 2 ^ fromIntegral n, 0, fromIntegral n)

{-
property: must sum up to 1
-}

{-
{- |
@eps@ is the threshold relatively to the maximum.
That is, if the gaussian falls below @eps * gaussian 0@,
then the function truncated.
-}
gaussian ::
   (Trans.C a, RealField.C a, Module.C a v) =>
   a -> a -> a -> sig v -> sig v
gaussian eps ratio freq =
   let var    = Filt.ratioFreqToVariance ratio freq
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
   (Trans.C a, RealField.C a, Module.C a v, SigG.Transform sig v) =>
   a -> a -> sig v -> sig v
binomial ratio freq =
   let width = ceiling (2 * Filt.ratioFreqToVariance ratio freq ^ 2)
   in  SigG.drop width .
       nest (2*width) (amplifyVector (asTypeOf 0.5 freq) . binomial1)

{-# INLINE binomial1 #-}
binomial1 ::
   (Additive.C v, SigG.Transform sig v) => sig v -> sig v
binomial1 = SigG.mapAdjacent (+)





{- |
Moving (uniformly weighted) average in the most trivial form.
This is very slow and needs about @n * length x@ operations.
-}
{-# INLINE sums #-}
sums ::
   (Additive.C v, SigG.Transform sig v) =>
   Int -> sig v -> sig v
sums n = SigG.mapTails (SigG.sum . SigG.take n)


sumsDownsample2 ::
   (Additive.C v, SigG.Write sig v) =>
   SigG.LazySize -> sig v -> sig v
sumsDownsample2 cs =
   SigG.unfoldR cs (\xs ->
      flip fmap (SigG.viewL xs) $ \xxs0@(x0,xs0) ->
         SigG.switchL xxs0 {- xs0 is empty -}
            (\ x1 xs1 -> (x0+x1, xs1))
            xs0)

downsample2 ::
   (SigG.Write sig v) =>
   SigG.LazySize -> sig v -> sig v
downsample2 cs =
   SigG.unfoldR cs
      (fmap (mapSnd SigG.laxTail) . SigG.viewL)

downsample ::
   (SigG.Write sig v) =>
   SigG.LazySize -> Int -> sig v -> sig v
downsample cs n =
   SigG.unfoldR cs
      (\xs -> fmap (mapSnd (const (SigG.drop n xs))) $ SigG.viewL xs)


{-
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
sumsUpsampleOdd :: (Additive.C v) => Int -> sig v -> sig v -> sig v
sumsUpsampleOdd n {- 2*k -} xs ss =
   let xs2k = drop n xs
   in  (head ss + head xs2k) :
          concat (zipWith3 (\s x0 x2k -> [x0+s, s+x2k])
                           (tail ss)
                           (downsample2 (tail xs))
                           (tail (downsample2 xs2k)))

sumsUpsampleEven :: (Additive.C v) => Int -> sig v -> sig v -> sig v
sumsUpsampleEven n {- 2*k -} xs ss =
   sumsUpsampleOdd (n+1) xs (zipWith (+) ss (downsample2 (drop n xs)))

sumsPyramid :: (Additive.C v) => Int -> sig v -> sig v
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

sumRange ::
   (Additive.C v, SigG.Transform sig v) =>
   sig v -> (Int,Int) -> v
sumRange =
   Filt.sumRangePrepare $ \ (l,r) ->
   SigG.sum . SigG.take (r-l) . SigG.drop l

pyramid ::
   (Additive.C v, SigG.Write sig v) =>
   Int -> sig v -> ([Int], [sig v])
pyramid height sig =
   let sizes = reverse $ take (1+height) $ iterate (2*) 1
   in  (sizes,
        scanl (flip sumsDownsample2) sig (map SigG.LazySize $ tail sizes))

{-# INLINE sumRangeFromPyramid #-}
sumRangeFromPyramid ::
   (Additive.C v, SigG.Transform sig v) =>
   [sig v] -> (Int,Int) -> v
sumRangeFromPyramid =
   Filt.sumRangePrepare $ \lr0 pyr0 ->
   consumeRangeFromPyramid (\v k s -> k (s+v)) id pyr0 lr0 zero

-- add from right to left, which is inefficient
sumRangeFromPyramidReverse ::
   (Additive.C v, SigG.Transform sig v) =>
   [sig v] -> (Int,Int) -> v
sumRangeFromPyramidReverse =
   Filt.sumRangePrepare $ \lr0 pyr0 ->
   consumeRangeFromPyramid (+) zero pyr0 lr0

-- for speed benchmarks
sumRangeFromPyramidFoldr ::
   (Additive.C v, SigG.Transform sig v) =>
   [sig v] -> (Int,Int) -> v
sumRangeFromPyramidFoldr =
   Filt.sumRangePrepare $ \lr0 pyr0 ->
   case pyr0 of
      [] -> error "empty pyramid"
      (ps0:pss) ->
         foldr
            (\psNext k (l,r) ps s ->
               case r-l of
                  0 -> s
                  1 -> s + SigG.index ps l
                  _ ->
                     let (lh,ll) = NP.negate $ divMod (NP.negate l) 2
                         (rh,rl) = divMod r 2
                         {-# INLINE inc #-}
                         inc b x = if b==0 then id else (x+)
                     in  k (lh,rh) psNext $
                         inc ll (SigG.index ps l) $
                         inc rl (SigG.index ps (r-1)) $
                         s)
            (\(l,r) ps s ->
               s + (SigG.sum $ SigG.take (r-l) $ SigG.drop l ps))
            pss lr0 ps0 zero

{-# INLINE maybeAccumulateRangeFromPyramid #-}
maybeAccumulateRangeFromPyramid ::
   (SigG.Transform sig v) =>
   (v -> v -> v) ->
   [sig v] -> (Int,Int) -> Maybe v
maybeAccumulateRangeFromPyramid acc =
   Filt.symmetricRangePrepare $ \lr0 pyr0 ->
   consumeRangeFromPyramid
      (\v k s -> k (fmap (acc v) s `mplus` Just v))
      id pyr0 lr0 Nothing

{-
This would also be a useful signature,
but that's not easy to implement
and I don't know whether it can be computed efficiently.

getRangeFromPyramid ::
   (Additive.C v, SigG.Transform sig v) =>
   [sig v] -> (Int,Int) -> SigS.T v
-}

{-# INLINE consumeRangeFromPyramid #-}
consumeRangeFromPyramid ::
   (SigG.Transform sig v) =>
   (v -> a -> a) -> a ->
   [sig v] -> (Int,Int) -> a
consumeRangeFromPyramid acc init0 pyr0 lr0 =
   case pyr0 of
      [] -> error "empty pyramid"
      (ps0:pss) ->
         foldr
            (\psNext k (l,r) ps ->
               case r-l of
                  0 -> init0
                  1 -> acc (SigG.index ps l) init0
                  _ ->
                     let (lh,ll) = NP.negate $ divMod (NP.negate l) 2
                         (rh,rl) = divMod r 2
                         {-# INLINE inc #-}
                         inc b x = if b==0 then id else acc x
                     in  inc ll (SigG.index ps l) $
                         inc rl (SigG.index ps (r-1)) $
                         k (lh,rh) psNext)
            (\(l,r) ps ->
               SigG.foldR acc init0 $
               SigG.take (r-l) $ SigG.drop l ps)
            pss lr0 ps0

sumsPosModulated ::
   (Additive.C v, SigG.Transform sig (Int,Int), SigG.Transform sig v) =>
   sig (Int,Int) -> sig v -> sig v
sumsPosModulated ctrl xs =
   SigG.zipWithTails (flip sumRange) ctrl xs


{- |
Moving average, where window bounds must be always non-negative.

The laziness granularity is @2^height@.
-}
{-# INLINE accumulatePosModulatedFromPyramid #-}
accumulatePosModulatedFromPyramid ::
   (SigG.Transform sig (Int,Int), SigG.Write sig v) =>
   ([sig v] -> (Int,Int) -> v) ->
   ([Int], [sig v]) ->
   sig (Int,Int) -> sig v
accumulatePosModulatedFromPyramid accumulate (sizes,pyr0) ctrl =
   let blockSize = head sizes
       pyrStarts = iterate (zipWith SigG.drop sizes) pyr0
       ctrlBlocks = SigS.toList $ SigG.sliceVertical blockSize ctrl
   in  SigG.concat $
       zipWith
          (\pyr ->
              SigG.fromState (SigG.LazySize blockSize) .
              SigS.map (accumulate pyr) .
              SigS.zipWith (\d -> mapPair ((d+), (d+))) (SigS.iterate (1+) 0) .
              SigG.toState)
          pyrStarts ctrlBlocks

sumsPosModulatedPyramid ::
   (Additive.C v, SigG.Transform sig (Int,Int), SigG.Write sig v) =>
   Int -> sig (Int,Int) -> sig v -> sig v
sumsPosModulatedPyramid height ctrl xs =
   accumulatePosModulatedFromPyramid
      sumRangeFromPyramid
      (pyramid height xs) ctrl

withPaddedInput ::
   (SigG.Transform sig Int, SigG.Transform sig (Int, Int),
    SigG.Write sig y) =>
   y -> (sig (Int, Int) -> sig y -> v) ->
   Int ->
   sig Int ->
   sig y -> v
withPaddedInput pad proc maxC ctrl xs =
   proc
      (SigG.map (\c -> (maxC - c, maxC + c + 1)) ctrl)
      (delayPad pad maxC xs)

{- |
The first argument is the amplification.
The main reason to introduce it,
was to have only a Module constraint instead of Field.
This way we can also filter stereo signals.
-}
movingAverageModulatedPyramid ::
   (Field.C a, Module.C a v,
    SigG.Transform sig Int, SigG.Transform sig (Int,Int), SigG.Write sig v) =>
   a -> Int -> Int -> sig Int -> sig v -> sig v
movingAverageModulatedPyramid amp height maxC ctrl0 =
   withPaddedInput zero
      (\ctrl xs ->
         SigG.zipWith (\c x -> (amp / fromIntegral (2*c+1)) *> x) ctrl0 $
         sumsPosModulatedPyramid height ctrl xs)
      maxC ctrl0


inverseFrequencyModulationFloor ::
   (Ord t, Ring.C t, SigG.Write sig v, SigG.Read sig t) =>
   SigG.LazySize ->
   sig t -> sig v -> sig v
inverseFrequencyModulationFloor chunkSize ctrl xs =
   SigG.fromState chunkSize
      (FiltS.inverseFrequencyModulationFloor
         (SigG.toState ctrl) (SigG.toState xs))



{- * Filter operators from calculus -}

{- |
Forward difference quotient.
Shortens the signal by one.
Inverts 'Synthesizer.Generic.Filter.Recursive.Integration.run' in the sense that
@differentiate (zero : integrate x) == x@.
The signal is shifted by a half time unit.
-}
{-# INLINE differentiate #-}
differentiate ::
   (Additive.C v, SigG.Transform sig v) =>
   sig v -> sig v
differentiate x = SigG.mapAdjacent subtract x

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
{-
This implementation is a bit cumbersome,
but it fits both StorableVector and State.Signal
(since it avoids recomputation).
-}
{-# INLINE differentiateCenter #-}
differentiateCenter ::
   (Field.C v, SigG.Transform sig v) =>
   sig v -> sig v
differentiateCenter =
   SigG.drop 2 .
   SigG.crochetL
      (\x0 (x1,x2) -> Just ((x2-x0)/2, (x0,x1)))
      (zero,zero)

{- |
Second derivative.
It is @differentiate2 == differentiate . differentiate@
but 'differentiate2' should be faster.
-}
{-# INLINE differentiate2 #-}
differentiate2 ::
   (Additive.C v, SigG.Transform sig v) =>
   sig v -> sig v
differentiate2 = differentiate . differentiate


-- * general non-recursive filters

{-|
Unmodulated non-recursive filter (convolution)

Brute force implementation.
-}
{-# INLINE generic #-}
generic ::
   (Module.C a v, SigG.Transform sig a, SigG.Write sig v) =>
   sig a -> sig v -> sig v
generic m x =
   if SigG.null m || SigG.null x
     then CutG.empty
     else
       let mr = SigG.reverse m
           xp = delayPos (pred (SigG.length m)) x
       in  SigG.mapTails (SigG.linearComb mr) xp


{- |
Both should signals should have similar length.
If they have considerably different length,
then better use 'karatsubaFiniteInfinite'.

Implementation using Karatsuba trick and split-and-overlap-add.
This way we stay in a ring, are faster than quadratic runtime
but do not reach log-linear runtime.
-}
karatsubaFinite ::
   (Additive.C a, Additive.C b, Additive.C c,
    SigG.Transform sig a, SigG.Transform sig b, SigG.Transform sig c) =>
   (a -> b -> c) ->
   sig a -> sig b -> sig c
karatsubaFinite mul a b =
   SigL.toSignal $
   karatsubaBounded mul
      (SigL.fromSignal a) (SigL.fromSignal b)

{-# INLINE karatsubaBounded #-}
karatsubaBounded ::
   (Additive.C a, Additive.C b, Additive.C c,
    SigG.Transform sig a, SigG.Transform sig b, SigG.Transform sig c) =>
   (a -> b -> c) ->
   SigL.T (sig a) -> SigL.T (sig b) -> SigL.T (sig c)
karatsubaBounded mul a b =
   case (SigL.length a, SigL.length b) of
      (0,_) -> CutG.empty
      (_,0) -> CutG.empty
      (1,_) ->
         SigG.switchL
            (error "karatsubaBounded: empty signal")
            (\y _ -> fmap (SigG.map (mul y)) b) $
         SigL.body a
      (_,1) ->
         SigG.switchL
            (error "karatsubaBounded: empty signal")
            (\y _ -> fmap (SigG.map (flip mul y)) a) $
         SigL.body b
      (2,2) ->
         let [a0,a1] = SigG.toList (SigL.toSignal a)
             [b0,b1] = SigG.toList (SigL.toSignal b)
             (c0,c1,c2) = convolvePair mul (a0,a1) (b0,b1)
         in  SigL.Cons 3 $ rechunk a b $
             c0 : c1 : c2 : []
      (2,3) ->
         let [a0,a1]    = SigG.toList (SigL.toSignal a)
             [b0,b1,b2] = SigG.toList (SigL.toSignal b)
             (c0,c1,c2,c3) =
                convolvePairTriple mul (a0,a1) (b0,b1,b2)
         in  SigL.Cons 4 $ rechunk a b $
             c0 : c1 : c2 : c3 : []
      (3,2) ->
         let [a0,a1,a2] = SigG.toList (SigL.toSignal a)
             [b0,b1]    = SigG.toList (SigL.toSignal b)
             (c0,c1,c2,c3) =
                convolvePairTriple (flip mul) (b0,b1) (a0,a1,a2)
         in  SigL.Cons 4 $ rechunk a b $
             c0 : c1 : c2 : c3 : []
      (3,3) ->
         let [a0,a1,a2] = SigG.toList (SigL.toSignal a)
             [b0,b1,b2] = SigG.toList (SigL.toSignal b)
             (c0,c1,c2,c3,c4) =
                convolveTriple mul (a0,a1,a2) (b0,b1,b2)
         in  SigL.Cons 5 $ rechunk a b $
             c0 : c1 : c2 : c3 : c4 : []
      (4,4) ->
         let [a0,a1,a2,a3] = SigG.toList (SigL.toSignal a)
             [b0,b1,b2,b3] = SigG.toList (SigL.toSignal b)
             (c0,c1,c2,c3,c4,c5,c6) =
                convolveQuadruple mul (a0,a1,a2,a3) (b0,b1,b2,b3)
         in  SigL.Cons 7 $ rechunk a b $
             c0 : c1 : c2 : c3 : c4 : c5 : c6 : []
      (lenA,lenB) ->
         let n2 = div (max lenA lenB) 2
             (a0,a1) = SigL.splitAt n2 a
             (b0,b1) = SigL.splitAt n2 b
             (c0,c1,c2) =
                convolvePair
                   (karatsubaBounded mul)
                   (a0,a1) (b0,b1)
         in  fmap (rechunk a b) $
             SigL.addShiftedSimple n2 c0 $
             SigL.addShiftedSimple n2 c1 c2

{-# INLINE rechunk #-}
rechunk ::
   (SigG.Transform sig1 a, SigG.Transform sig1 b, SigG.Transform sig1 c,
    SigG.Transform sig0 c) =>
   SigL.T (sig1 a) -> SigL.T (sig1 b) -> sig0 c -> sig1 c
rechunk a b c =
   let (ac,bc) = CutG.splitAt (SigL.length a) c
   in  SigG.takeStateMatch (SigL.body a) (SigG.toState ac)
       `SigG.append`
       SigG.takeStateMatch (SigL.body b) (SigG.toState bc)


{- |
The first operand must be finite and
the second one can be infinite.
For efficient operation we expect that the second signal
is longer than the first one.
-}
{-
Implemented by overlap-add of pieces that are convolved by Karatsuba trick.
Is it more efficient to round the chunk size up to the next power of two?
Can we make use of the fact,
that the first operand is always split in the same way?
-}
karatsubaFiniteInfinite ::
   (Additive.C a, Additive.C b, Additive.C c,
    SigG.Transform sig a, SigG.Transform sig b, SigG.Transform sig c) =>
   (a -> b -> c) ->
   sig a -> sig b -> sig c
karatsubaFiniteInfinite mul a b =
   let al = SigL.fromSignal a
   in  case SigL.length al of
          0 -> CutG.empty
          alen ->
             SigS.foldR (addShiftedSimple alen) CutG.empty $
             SigS.map SigL.toSignal $
             SigS.map (karatsubaBounded mul al . SigL.fromSignal) $
             SigG.sliceVertical alen b


karatsubaInfinite ::
   (Additive.C a, Additive.C b, Additive.C c,
    SigG.Transform sig a, SigG.Transform sig c, SigG.Transform sig b) =>
   (a -> b -> c) ->
   sig a -> sig b -> sig c
karatsubaInfinite mul =
   let recourse n a b =
          let (a0,a1) = SigG.splitAt n a
              (b0,b1) = SigG.splitAt n b
              {-
              We could also apply Karatsuba's trick to these pairs.
              But this requires Additive (sig a) constraint
              and I do not know whether this is actually an optimization.
              -}
              ab00 =
                 SigL.toSignal $
                 karatsubaBounded mul
                    (SigL.fromSignal a0) (SigL.fromSignal b0)
              ab01 = karatsubaFiniteInfinite mul a0 b1
              ab10 = karatsubaFiniteInfinite (flip mul) b0 a1
              ab11 = recourse (2*n) a1 b1
          in  if SigG.null a || SigG.null b
                then CutG.empty
                else
                  addShiftedSimple n ab00 $
                  addShiftedSimple n (SigG.mix ab01 ab10) ab11
   in  recourse 1


{- |
It must hold @delay <= length a@.
-}
{-
It is crucial that 'mix' uses the chunk size structure of the second operand.
This way we avoid unnecessary and even infinite look-ahead.
-}
{-# INLINE addShiftedSimple #-}
addShiftedSimple ::
   (Additive.C a, SigG.Transform sig a) =>
   Int -> sig a -> sig a -> sig a
addShiftedSimple del a b =
   uncurry CutG.append $
   mapSnd (flip SigG.mix b) $
   CutG.splitAt del a


-- ** hard-wired convolutions for small sizes

{-
Some small size convolutions using the Karatsuba trick.
We do not use Toom-3 multiplication,
because this requires division by 2 and 6.
With Karatsuba we can stay in a ring.
-}

type Pair a = (a,a)

{- |
Reasonable choices for the multiplication operation are '(*)', '(*>)', 'convolve'.
-}
{-# INLINE convolvePair #-}
convolvePair ::
   (Additive.C a, Additive.C b, Additive.C c) =>
   (a -> b -> c) ->
   Pair a -> Pair b -> Triple c
convolvePair mul a b =
   snd $ sumAndConvolvePair mul a b

{-# INLINE sumAndConvolvePair #-}
sumAndConvolvePair ::
   (Additive.C a, Additive.C b, Additive.C c) =>
   (a -> b -> c) ->
   Pair a -> Pair b -> ((a,b), Triple c)
sumAndConvolvePair (!*!) (a0,a1) (b0,b1) =
   let sa01 = a0+a1
       sb01 = b0+b1
       ab0 = a0!*!b0
       ab1 = a1!*!b1
   in  ((sa01, sb01), (ab0, sa01!*!sb01-(ab0+ab1), ab1))

type Triple a = (a,a,a)

{-# INLINE convolvePairTriple #-}
convolvePairTriple ::
   (Additive.C a, Additive.C b, Additive.C c) =>
   (a -> b -> c) ->
   Pair a -> Triple b -> (c,c,c,c)
convolvePairTriple (!*!) (a0,a1) (b0,b1,b2) =
   let ab0 = a0!*!b0
       ab1 = a1!*!b1
       sa01 = a0+a1; sb01 = b0+b1; ab01 = sa01!*!sb01
   in  (ab0, ab01 - (ab0+ab1),
        a0!*!b2 + ab1, a1!*!b2)


{-# INLINE convolveTriple #-}
convolveTriple ::
   (Additive.C a, Additive.C b, Additive.C c) =>
   (a -> b -> c) ->
   Triple a -> Triple b -> (c,c,c,c,c)
convolveTriple mul a b =
   snd $ sumAndConvolveTriple mul a b

{-# INLINE sumAndConvolveTriple #-}
sumAndConvolveTriple ::
   (Additive.C a, Additive.C b, Additive.C c) =>
   (a -> b -> c) ->
   Triple a -> Triple b -> ((a,b), (c,c,c,c,c))
sumAndConvolveTriple (!*!) (a0,a1,a2) (b0,b1,b2) =
   let ab0 = a0!*!b0
       ab1 = a1!*!b1
       ab2 = a2!*!b2
       sa01 = a0+a1; sb01 = b0+b1; ab01 = sa01!*!sb01
       sa02 = a0+a2; sb02 = b0+b2; ab02 = sa02!*!sb02
       sa012 = sa01+a2
       sb012 = sb01+b2
   in  ((sa012, sb012),
        (ab0, ab01 - (ab0+ab1),
         ab02 + ab1 - (ab0+ab2),
         sa012!*!sb012 - ab02 - ab01 + ab0, ab2))

{-# INLINE sumAndConvolveTripleAlt #-}
sumAndConvolveTripleAlt ::
   (Additive.C a, Additive.C b, Additive.C c) =>
   (a -> b -> c) ->
   Triple a -> Triple b -> ((a,b), (c,c,c,c,c))
sumAndConvolveTripleAlt (!*!) (a0,a1,a2) (b0,b1,b2) =
   let ab0 = a0!*!b0
       ab1 = a1!*!b1
       ab2 = a2!*!b2
       sa01 = a0+a1; sb01 = b0+b1
       ab01 = sa01!*!sb01 - (ab0+ab1)
       sa02 = a0+a2; sb02 = b0+b2
       ab02 = sa02!*!sb02 - (ab0+ab2)
       sa12 = a1+a2; sb12 = b1+b2
       ab12 = sa12!*!sb12 - (ab1+ab2)
   in  ((sa01+a2, sb01+b2),
        (ab0, ab01, ab1+ab02, ab12, ab2))

type Quadruple a = (a,a,a,a)

{-# INLINE convolveQuadruple #-}
convolveQuadruple ::
   (Additive.C a, Additive.C b, Additive.C c) =>
   (a -> b -> c) ->
   Quadruple a -> Quadruple b -> (c,c,c,c,c,c,c)
convolveQuadruple mul a b =
   snd $ sumAndConvolveQuadruple mul a b

{-# INLINE sumAndConvolveQuadruple #-}
sumAndConvolveQuadruple ::
   (Additive.C a, Additive.C b, Additive.C c) =>
   (a -> b -> c) ->
   Quadruple a -> Quadruple b -> ((a,b), (c,c,c,c,c,c,c))
sumAndConvolveQuadruple (!*!) (a0,a1,a2,a3) (b0,b1,b2,b3) =
   let ab0 = a0!*!b0
       ab1 = a1!*!b1
       sa01 = a0+a1; sb01 = b0+b1
       ab01 = sa01!*!sb01 - (ab0+ab1)
       ab2 = a2!*!b2
       ab3 = a3!*!b3
       sa23 = a2+a3; sb23 = b2+b3
       ab23 = sa23!*!sb23 - (ab2+ab3)
       ab02 = (a0+a2)!*!(b0+b2)
       ab13 = (a1+a3)!*!(b1+b3)
       sa0123 = sa01+sa23
       sb0123 = sb01+sb23
       ab0123 = sa0123!*!sb0123 - (ab02+ab13)
   in  ((sa0123, sb0123),
        (ab0, ab01, ab1+ab02-(ab0+ab2),
         ab0123 - (ab01+ab23),
         ab2+ab13-(ab1+ab3), ab23, ab3))

{-# INLINE sumAndConvolveQuadrupleAlt #-}
sumAndConvolveQuadrupleAlt ::
   (Additive.C a, Additive.C b, Additive.C c) =>
   (a -> b -> c) ->
   Quadruple a -> Quadruple b -> ((a,b), (c,c,c,c,c,c,c))
sumAndConvolveQuadrupleAlt mul (a0,a1,a2,a3) (b0,b1,b2,b3) =
   let (((sa02,sa13), (sb02,sb13)),
        ((c00,c01,c02), (c10,c11,c12), (c20,c21,c22))) =
          sumAndConvolvePair (convolvePair mul)
             ((a0,a1),(a2,a3)) ((b0,b1),(b2,b3))
   in  ((sa02+sa13, sb02+sb13),
        (c00,c01,c02+c10,c11,c12+c20,c21,c22))
