module CorrelateResample where

import Vector (findPeak, mulConj)
import Common (cyclicFromVector, pad)

import qualified Numeric.FFTW.Shape as Spectrum
import qualified Numeric.FFTW.Rank1 as Trafo1
import qualified Numeric.Netlib.Class as Class

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.StorableVector.Lazy as SVL
import Data.Array.Comfort.Storable.Unchecked (Array)
import Data.Array.Comfort.Shape ((::+)((::+)))

import Algebra.IntegralDomain (divUp)


adjust ::
   (Shape.C sh0, Shape.C sh1, Class.Floating a) =>
   sh1 -> Array sh0 a -> Array sh1 a
adjust sh1 xs =
   let n0 = Shape.size $ Array.shape xs
       n1 = Shape.size sh1
   in case compare n0 n1 of
         EQ -> Array.reshape sh1 xs
         GT ->
            Array.takeLeft $ Array.reshape (sh1::+Shape.ZeroBased (n0-n1)) xs
         LT ->
            Array.reshape sh1 $ Array.append xs $
            Array.fromAssociations 0 (Shape.ZeroBased (n1-n0)) []

{- |
Resample and correlate input vectors.
We pad the inputs to a multiple of whole seconds.
We perform resampling implicitly by cutting the spectrum.
-}
correlate ::
   (Class.Real a) =>
   Integer ->
   (Integer, SVL.Vector a) -> (Integer, SVL.Vector a) ->
   Array (Shape.Cyclic Int) a
correlate dstRate (xrate,xs) (yrate,ys) =
   let nx = fromIntegral $ SVL.length xs
       ny = fromIntegral $ SVL.length ys
       seconds = fromInteger $ divUp (nx*yrate + ny*xrate) (xrate*yrate)
       paddedAtRate r = seconds * fromInteger r
       shz = Spectrum.Half $ paddedAtRate dstRate
   in Trafo1.fourierCR $
      mulConj
         (adjust shz $ Trafo1.fourierRC $
          cyclicFromVector $ pad (paddedAtRate xrate) xs)
         (adjust shz $ Trafo1.fourierRC $
          cyclicFromVector $ pad (paddedAtRate yrate) ys)

determineLag ::
   (Class.Real a) =>
   Integer -> (Integer, SVL.Vector a) -> (Integer, SVL.Vector a) -> Int
determineLag dstRate xt@(xrate,xs) yt =
   let zs = correlate dstRate xt yt
   in case findPeak zs of
         (neg,nonNeg) ->
            if fromIntegral nonNeg * xrate >=
                  fromIntegral (SVL.length xs) * dstRate
               then neg else nonNeg
