module Correlate where

import Vector (findPeak, mulConj)
import Common (cyclicFromVector, pad)

import qualified Numeric.FFTW.Rank1 as Trafo1
import qualified Numeric.Netlib.Class as Class

import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.StorableVector.Lazy as SVL
import Data.Array.Comfort.Storable (Array)

import qualified Data.Stream as Stream


{- |
Round to next higher number of the form m*2^n with m<16.
This size contains almost only factor 2 and at most one ugly factor < 16
and we add at most 7% pad data.

>>> map ceilingFFTSize [1..40]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,18,20,20,22,22,24,24,26,26,28,28,30,30,32,32,36,36,36,36,40,40,40,40]
prop> \n -> n <= ceilingFFTSize n
prop> \n -> ceilingFFTSize n <= div (n * 107) 100
-}
ceilingFFTSize :: Int -> Int
ceilingFFTSize n =
   case Stream.dropWhile ((>=16).fst) $
        Stream.zip
            (Stream.iterate (\k -> div (k+1) 2) n)
            (Stream.iterate (2*) 1) of
      Stream.Cons (m,p) _ -> m*p


correlate ::
   (Class.Real a) =>
   SVL.Vector a -> SVL.Vector a -> Array (Shape.Cyclic Int) a
correlate xs ys =
   let nx = SVL.length xs
       ny = SVL.length ys
       n = ceilingFFTSize (nx+ny)
   in Trafo1.fourierCR $
      mulConj
         (Trafo1.fourierRC (cyclicFromVector (pad n xs)))
         (Trafo1.fourierRC (cyclicFromVector (pad n ys)))

determineLag :: (Class.Real a) => SVL.Vector a -> SVL.Vector a -> Int
determineLag xs ys =
   let zs = correlate xs ys
   in case findPeak zs of
         (neg,nonNeg) -> if nonNeg >= SVL.length xs then neg else nonNeg
