module Numeric.FFTW.Rank2 (
   fourier, Sign(..), flipSign,
   fourierRC,
   fourierCR,
   ) where

import qualified Numeric.FFTW.Shape as Spectrum
import qualified Numeric.FFTW.FFI as FFI
import qualified Numeric.Netlib.Class as Class
import Numeric.FFTW.Private (Sign(..), flipSign, ffiSign, run, runCopiedArray)

import Foreign.Marshal.Array (pokeArray)
import Foreign.ForeignPtr (withForeignPtr)

import qualified Data.Array.Comfort.Storable.Unchecked as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Unchecked (Array(Array))

import Data.Complex (Complex)
import Data.Monoid ((<>))

import Control.Monad (when)


{- $setup
>>> import Test.Numeric.FFTW.Common
>>>    (approxReal, approxComplex, floatTol, doubleTol, normInf,
>>>     adjust, scalarProduct, split, genCyclicArray2, immutable,
>>>     arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble)
>>>
>>> import qualified Numeric.FFTW.Rank2 as Trafo2
>>> import Numeric.FFTW.Rank1 (flipSign)
>>>
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Complex as Complex
>>>
>>> import qualified Test.QuickCheck as QC
-}


{- |
prop> QC.forAll genCyclicArray2 $ \xs sign-> approxComplex floatTol (adjust xs) (Trafo2.fourier sign (Trafo2.fourier (flipSign sign) xs))
prop> QC.forAll genCyclicArray2 $ \xs sign -> approxComplex doubleTol (adjust xs) (Trafo2.fourier sign (Trafo2.fourier (flipSign sign) xs))

prop> QC.forAll genCyclicArray2 $ \xs sign -> approxComplex floatTol (Trafo2.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo2.fourier (flipSign sign) xs))
prop> QC.forAll genCyclicArray2 $ \xs sign -> approxComplex doubleTol (Trafo2.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo2.fourier (flipSign sign) xs))

prop> QC.forAll genCyclicArray2 $ \xys sign -> let (xs,ys) = split xys in approxComplex floatTol (Trafo2.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo2.fourier sign xs) (Trafo2.fourier sign ys))
prop> QC.forAll genCyclicArray2 $ \xys sign -> let (xs,ys) = split xys in approxComplex doubleTol (Trafo2.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo2.fourier sign xs) (Trafo2.fourier sign ys))

prop> QC.forAll genCyclicArray2 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo2.fourier sign xs) (Trafo2.fourier sign ys)) <= floatTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys
prop> QC.forAll genCyclicArray2 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo2.fourier sign xs) (Trafo2.fourier sign ys)) <= doubleTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys

prop> \sign -> QC.forAll genCyclicArray2 $ immutable (Trafo2.fourier sign) . arrayComplexFloat
prop> \sign -> QC.forAll genCyclicArray2 $ immutable (Trafo2.fourier sign) . arrayComplexDouble
-}
fourier ::
   (Integral n0, Integral n1, Class.Real a) =>
   Sign ->
   Array (Shape.Cyclic n0, Shape.Cyclic n1) (Complex a) ->
   Array (Shape.Cyclic n0, Shape.Cyclic n1) (Complex a)
fourier sign (Array sh@(Shape.Cyclic n0, Shape.Cyclic n1) x) =
   Array.unsafeCreateWithSize sh $ \n yPtr ->
   withForeignPtr x $ \xPtr ->
   when (n>0) $ run $
      FFI.planDFT2d (fromIntegral n0) (fromIntegral n1)
         xPtr yPtr (ffiSign sign) (FFI.estimate <> FFI.preserveInput)

{- |
prop> QC.forAll genCyclicArray2 $ \xs -> approxReal floatTol (adjust xs) (Trafo2.fourierCR (Trafo2.fourierRC xs))
prop> QC.forAll genCyclicArray2 $ \xs -> approxReal doubleTol (adjust xs) (Trafo2.fourierCR (Trafo2.fourierRC xs))

prop> QC.forAll genCyclicArray2 $ immutable Trafo2.fourierRC . arrayFloat
prop> QC.forAll genCyclicArray2 $ immutable Trafo2.fourierRC . arrayDouble
-}
fourierRC ::
   (Integral n0, Integral n1, Class.Real a) =>
   Array (Shape.Cyclic n0, Shape.Cyclic n1) a ->
   Array (Shape.Cyclic n0, Spectrum.Half n1) (Complex a)
fourierRC (Array (sh0@(Shape.Cyclic n0), Shape.Cyclic n1) x) =
   Array.unsafeCreate (sh0, Spectrum.Half n1) $ \yPtr ->
   withForeignPtr x $ \xPtr ->
   if n0<=0 || n1<=0
      then pokeArray yPtr $ replicate (fromIntegral n0) 0
      else run $
         FFI.planDFTr2c2d (fromIntegral n0) (fromIntegral n1)
            xPtr yPtr (FFI.estimate <> FFI.preserveInput)

{- |
prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable Trafo2.fourierCR . arrayComplexFloat
prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable Trafo2.fourierCR . arrayComplexDouble
-}
fourierCR ::
   (Integral n0, Integral n1, Class.Real a) =>
   Array (Shape.Cyclic n0, Spectrum.Half n1) (Complex a) ->
   Array (Shape.Cyclic n0, Shape.Cyclic n1) a
fourierCR arr@(Array (sh0@(Shape.Cyclic n0), Spectrum.Half n1) _x) =
   Array.unsafeCreate (sh0, Shape.Cyclic n1) $ \yPtr ->
   when (n0>0 && n1>0) $
   runCopiedArray arr $ \xPtr ->
      FFI.planDFTc2r2d (fromIntegral n0) (fromIntegral n1)
         xPtr yPtr (FFI.estimate <> FFI.destroyInput)
