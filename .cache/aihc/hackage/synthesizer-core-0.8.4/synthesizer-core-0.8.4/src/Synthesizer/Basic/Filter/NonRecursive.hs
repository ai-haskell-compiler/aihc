{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Basic.Filter.NonRecursive where

import qualified Data.List.Match as Match
import Data.Tuple.HT (sortPair, )

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Additive       as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


unitSizesFromPyramid :: [signal] -> [Int]
unitSizesFromPyramid pyr =
   reverse $ Match.take pyr $ iterate (2*) 1

sumRangePrepare :: (Additive.C v) =>
   ((Int,Int) -> source -> v) ->
   (source -> (Int,Int) -> v)
sumRangePrepare f pyr (l,r) =
   case compare l r of
      LT -> f (l,r) pyr
      GT -> negate $ f (r,l) pyr
      EQ -> zero

symmetricRangePrepare ::
   ((Int,Int) -> source -> v) ->
   (source -> (Int,Int) -> v)
symmetricRangePrepare f pyr lr = f (sortPair lr) pyr

{-
exp (-(t/var)^2/2) / area *> cis (2*pi*f*t)
  == exp (-(t/var)^2/2 +: 2*pi*f*t) / area
  == exp ((-t^2 +: 2*var^2*2*pi*f*t) / (2*var^2)) / area
  == exp ((t^2 - i*2*var^2*2*pi*f*t) / (-2*var^2)) / area
  == exp (((t^2 - i*var^2*2*pi*f)^2 + (var^2*2*pi*f)^2) / (-2*var^2)) / area
  == exp (((t^2 - i*var^2*2*pi*f)^2 / (-2*var^2) - (var*2*pi*f)^2/2)) / area

sumMap (\t -> exp (-(t/var)^2/2) / area *> cis (2*pi*f*t))
       [-infinity..infinity]
  ~ sumMap (\t -> exp (-(t/var)^2/2)) [-infinity..infinity]
       * exp (-(var*2*pi*f)^2/2) / area
  = exp (-(var*2*pi*f)^2/2)
-}
{- |
  Compute the variance of the Gaussian
  such that its Fourier transform has value @ratio@ at frequency @freq@.
-}
ratioFreqToVariance :: (Trans.C a) => a -> a -> a
ratioFreqToVariance ratio freq =
   sqrt (-2 * log ratio) / (2*pi*freq)
           -- inverse of the fourier transformed gaussian
