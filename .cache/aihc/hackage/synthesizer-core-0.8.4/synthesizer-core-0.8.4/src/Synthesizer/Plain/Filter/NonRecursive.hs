{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2006-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Plain.Filter.NonRecursive (
   amplify,
   amplifyVector,
   binomial,
   binomial1,
   delay,
   delayPad,
   differentiate,
   differentiate2,
   differentiateCenter,
   downsample2,
   envelope,
   envelopeVector,
   fadeInOut,
   fadeInOutAlt,
   gaussian,
   generic,
   genericAlt,
   minRange,
   movingAverageModulatedPyramid,
   sumRange,
   sumRangeFromPyramid,
   sums,
   sumsDownsample2,
   sumsPosModulated,
   sumsPosModulatedPyramid,
   sumsPyramid,

   -- for testing
   propGeneric,
   sumRangeFromPyramidFoldr,
   sumRangeFromPyramidRec,
   getRangeFromPyramid,
   pyramid,
   ) where

import Synthesizer.Basic.Filter.NonRecursive (
   unitSizesFromPyramid,
   sumRangePrepare,
   symmetricRangePrepare,
   ratioFreqToVariance,
   )

import qualified Synthesizer.Plain.Control as Ctrl
import qualified Synthesizer.Plain.Signal  as Sig

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import Algebra.Module(linearComb, )

import Data.Function.HT (nest, )
import Data.Tuple.HT (mapPair, )
import Data.List.HT (sliceVertical, )
import Data.List (tails, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Envelope application -}

amplify :: (Ring.C a) => a -> Sig.T a -> Sig.T a
amplify v = map (v*)

amplifyVector :: (Module.C a v) => a -> Sig.T v -> Sig.T v
amplifyVector = (*>)


envelope :: (Ring.C a) =>
      Sig.T a  {-^ the envelope -}
   -> Sig.T a  {-^ the signal to be enveloped -}
   -> Sig.T a
envelope = zipWith (*)

envelopeVector :: (Module.C a v) =>
      Sig.T a  {-^ the envelope -}
   -> Sig.T v  {-^ the signal to be enveloped -}
   -> Sig.T v
envelopeVector = zipWith (*>)



fadeInOut :: (Field.C a) => Int -> Int -> Int -> Sig.T a -> Sig.T a
fadeInOut tIn tHold tOut xs =
   let leadIn  = take tIn  $ Ctrl.linearMultiscale (  recip (fromIntegral tIn))  0
       leadOut = take tOut $ Ctrl.linearMultiscale (- recip (fromIntegral tOut)) 1
       (partIn, partHoldOut) = splitAt tIn xs
       (partHold, partOut) = splitAt tHold partHoldOut
   in  envelope leadIn partIn ++
       partHold ++
       envelope leadOut partOut


fadeInOutAlt :: (Field.C a) => Int -> Int -> Int -> Sig.T a -> Sig.T a
fadeInOutAlt tIn tHold tOut =
   zipWith id
      ((map (\x y -> y * fromIntegral x / fromIntegral tIn) [0..tIn-1]) ++
       (replicate tHold id) ++
       (map (\x y -> y * fromIntegral x / fromIntegral tOut) [tOut-1,tOut-2..0]))



{- * Shift -}

delay :: Additive.C y => Int -> Sig.T y -> Sig.T y
delay = delayPad zero

delayPad :: y -> Int -> Sig.T y -> Sig.T y
delayPad z n =
   if n<0
     then drop (negate n)
     else (replicate n z ++)


{- * Smoothing -}


{-| Unmodulated non-recursive filter -}
generic :: Module.C a v =>
   Sig.T a -> Sig.T v -> Sig.T v
generic m x =
   let mr = reverse m
       xp = delay (pred (length m)) x
   in  map (linearComb mr) (init (tails xp))

{-|
Unmodulated non-recursive filter
Output has same length as the input.

It is elegant but leaks memory.
 -}
genericAlt :: Module.C a v =>
   Sig.T a -> Sig.T v -> Sig.T v
genericAlt m x =
   map (linearComb m)
      (tail (scanl (flip (:)) [] x))


propGeneric :: (Module.C a v, Eq v) =>
   Sig.T a -> Sig.T v -> Bool
propGeneric m x =
--   generic m x == genericAlt m x
   and $ zipWith (==) (generic m x) (genericAlt m x)



{- |
@eps@ is the threshold relatively to the maximum.
That is, if the gaussian falls below @eps * gaussian 0@,
then the function truncated.
-}
gaussian :: (Trans.C a, RealField.C a, Module.C a v) => a -> a -> a -> Sig.T v -> Sig.T v
gaussian eps ratio freq =
   let var    = ratioFreqToVariance ratio freq
       area   = var * sqrt (2*pi)
       gau t  = exp (-(t/var)^2/2) / area
       width  = ceiling (var * sqrt (-2 * log eps))  -- inverse gau
       gauSmp = map (gau . fromIntegral) [-width .. width]
   in  drop width . generic gauSmp

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
binomial :: (Trans.C a, RealField.C a, Module.C a v) => a -> a -> Sig.T v -> Sig.T v
binomial ratio freq =
   let width = ceiling (2 * ratioFreqToVariance ratio freq ^ 2)
   in  drop width . nest (2*width) ((asTypeOf 0.5 freq *>) . binomial1)

binomial1 :: (Additive.C v) => Sig.T v -> Sig.T v
binomial1 xt@(x:xs) = x : (xs + xt)
binomial1 [] = []






{- |
Moving (uniformly weighted) average in the most trivial form.
This is very slow and needs about @n * length x@ operations.
-}
sums :: (Additive.C v) => Int -> Sig.T v -> Sig.T v
sums n = map (sum . take n) . init . tails



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

Example for 2*k+1

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
sumsPyramid =
   let aux 1 ys = ys
       aux 2 ys = ys + tail ys  -- binomial1
       aux m ys =
          let ysd = sumsDownsample2 ys
          in  if even m
                then sumsUpsampleEven (m-2) ys (aux (div (m-2) 2) ysd)
                else sumsUpsampleOdd  (m-1) ys (aux (div (m-1) 2) ysd)
   in  aux


{- |
Compute the sum of the values from index l to (r-1).
(I.e. somehow a right open interval.)
This can be used for implementation of a moving average filter.
However, its counterpart 'sumRangeFromPyramid'
is much faster for large windows.
-}
sumRange :: (Additive.C v) => Sig.T v -> (Int,Int) -> v
sumRange =
   sumRangePrepare $ \ (l,r) ->
   sum . take (r-l) . drop l

pyramid :: (Additive.C v) => Sig.T v -> [Sig.T v]
pyramid = iterate sumsDownsample2

{- |
This function should be much faster than 'sumRange'
but slower than the recursively implemented @movingAverage@.
However in contrast to @movingAverage@
it should not suffer from cancelation.
-}
sumRangeFromPyramid :: (Additive.C v) => [Sig.T v] -> (Int,Int) -> v
sumRangeFromPyramid =
   sumRangePrepare $ \lr0 pyr0 ->
   sum $ getRangeFromPyramid pyr0 lr0

minRange :: (Ord v) => Sig.T v -> (Int,Int) -> v
minRange =
   symmetricRangePrepare $ \ (l,r) ->
   minimum . take (r-l) . drop l

getRangeFromPyramid :: [Sig.T v] -> (Int,Int) -> [v]
getRangeFromPyramid pyr0 lr0 =
   case pyr0 of
      [] -> error "empty pyramid"
      (ps0:pss) ->
         foldr
            (\psNext k ((l,r), ps) ->
               let (lh,ll) = - divMod (-l) 2
                   (rh,rl) =   divMod   r  2
                   ls = if ll==0 then [] else [ps!!l]
                   rs = if rl==0 then [] else [ps!!(r-1)]
               in  case r-l of
                     0 -> []
                     1 -> [ps!!l]
                     _ -> ls ++ rs ++ k ((lh,rh), psNext))
            (\((l,r), ps) -> take (r-l) $ drop l ps)
            pss (lr0, ps0)

{- mapAccumL cannot work since the pyramid might be infinitely high
   sum $ takeWhileJust $ snd $
   mapAccumL () lr0 $
   zip pyr0 $
   tail (Match.replicate pyr0 False ++ [True])
-}

sumRangeFromPyramidRec ::
   (Additive.C v) =>
   [Sig.T v] -> (Int,Int) -> v
sumRangeFromPyramidRec =
   let recourse s (l,r) pyr =
          case pyr of
             (ps:[]) ->
                s + (sum $ take (r-l) $ drop l ps)
             (ps:pss) ->
                let (lh,ll) = - divMod (-l) 2
                    (rh,rl) =   divMod   r  2
                    ls = if ll==0 then zero else ps!!l
                    rs = if rl==0 then zero else ps!!(r-1)
                in  case r-l of
                      0 -> s
                      1 -> s+ps!!l
                      _ -> recourse (s + ls + rs) (lh,rh) pss
             [] -> error "empty pyramid"
   in  sumRangePrepare (recourse zero)

sumRangeFromPyramidFoldr ::
   (Additive.C v) =>
   [Sig.T v] -> (Int,Int) -> v
sumRangeFromPyramidFoldr =
   sumRangePrepare $ \lr0 pyr0 ->
   case pyr0 of
      [] -> error "empty pyramid"
      (ps0:pss) ->
         foldr
            (\psNext k (l,r) ps s ->
               case r-l of
                  0 -> s
                  1 -> s + ps!!l
                  _ ->
                     let (lh,ll) = - divMod (-l) 2
                         (rh,rl) =   divMod   r  2
                         {-# INLINE inc #-}
                         inc b x = if b==0 then id else (x+)
                     in  k (lh,rh) psNext $
                         inc ll (ps!!l) $
                         inc rl (ps!!(r-1)) $
                         s)
            (\(l,r) ps s ->
               s + (sum $ take (r-l) $ drop l ps))
            pss lr0 ps0 zero

sumsPosModulated :: (Additive.C v) =>
   Sig.T (Int,Int) -> Sig.T v -> Sig.T v
sumsPosModulated ctrl xs =
   zipWith sumRange (tails xs) ctrl

{- |
Moving average, where window bounds must be always non-negative.

The laziness granularity is @2^height@.
-}
sumsPosModulatedPyramid :: (Additive.C v) =>
   Int -> Sig.T (Int,Int) -> Sig.T v -> Sig.T v
sumsPosModulatedPyramid height ctrl xs =
   let blockSize = 2 ^ fromIntegral height
       pyr0 = take (1+height) $ pyramid xs
       sizes = unitSizesFromPyramid pyr0
       pyrStarts =
          iterate (zipWith drop sizes) pyr0
       ctrlBlocks =
          map (zipWith (\d -> mapPair ((d+), (d+))) $ iterate (1+) 0) $
          sliceVertical blockSize ctrl
   in  concat $
       zipWith
          (\pyr -> map (sumRangeFromPyramid pyr))
          pyrStarts ctrlBlocks

{- |
The first argument is the amplification.
The main reason to introduce it,
was to have only a Module constraint instead of Field.
This way we can also filter stereo signals.

A control value @n@ corresponds to filter window size @2*n+1@.
-}
movingAverageModulatedPyramid ::
   (Field.C a, Module.C a v) =>
   a -> Int -> Int -> Sig.T Int -> Sig.T v -> Sig.T v
movingAverageModulatedPyramid amp height maxC ctrl xs =
   zipWith (\c x -> (amp / fromIntegral (2*c+1)) *> x) ctrl $
   sumsPosModulatedPyramid height
      (map (\c -> (maxC - c, maxC + c + 1)) ctrl)
      (delay maxC xs)



{- * Filter operators from calculus -}

{- |
Forward difference quotient.
Shortens the signal by one.
Inverts 'Synthesizer.Plain.Filter.Recursive.Integration.run' in the sense that
@differentiate (zero : integrate x) == x@.
The signal is shifted by a half time unit.
-}
differentiate :: Additive.C v => Sig.T v -> Sig.T v
differentiate x = zipWith subtract x (tail x)

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
differentiateCenter :: Field.C v => Sig.T v -> Sig.T v
differentiateCenter x =
   map ((1/2)*) $
   zipWith subtract x (tail (tail x))

{- |
Second derivative.
It is @differentiate2 == differentiate . differentiate@
but 'differentiate2' should be faster.
-}
differentiate2 :: Additive.C v => Sig.T v -> Sig.T v
differentiate2 xs0 =
   let xs1 = tail xs0
       xs2 = tail xs1
   in  zipWith3 (\x0 x1 x2 -> x0+x2-(x1+x1)) xs0 xs1 xs2
