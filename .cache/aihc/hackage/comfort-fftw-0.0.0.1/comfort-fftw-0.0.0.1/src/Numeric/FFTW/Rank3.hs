module Numeric.FFTW.Rank3 (
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
>>>     adjust, scalarProduct, split, genCyclicArray3, immutable,
>>>     arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble)
>>>
>>> import qualified Numeric.FFTW.Rank3 as Trafo3
>>> import Numeric.FFTW.Rank1 (flipSign)
>>>
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Complex as Complex
>>>
>>> import qualified Test.QuickCheck as QC
-}


{- |
prop> QC.forAll genCyclicArray3 $ \xs sign-> approxComplex floatTol (adjust xs) (Trafo3.fourier sign (Trafo3.fourier (flipSign sign) xs))
prop> QC.forAll genCyclicArray3 $ \xs sign -> approxComplex doubleTol (adjust xs) (Trafo3.fourier sign (Trafo3.fourier (flipSign sign) xs))

prop> QC.forAll genCyclicArray3 $ \xs sign -> approxComplex floatTol (Trafo3.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo3.fourier (flipSign sign) xs))
prop> QC.forAll genCyclicArray3 $ \xs sign -> approxComplex doubleTol (Trafo3.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo3.fourier (flipSign sign) xs))

prop> QC.forAll genCyclicArray3 $ \xys sign -> let (xs,ys) = split xys in approxComplex floatTol (Trafo3.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo3.fourier sign xs) (Trafo3.fourier sign ys))
prop> QC.forAll genCyclicArray3 $ \xys sign -> let (xs,ys) = split xys in approxComplex doubleTol (Trafo3.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo3.fourier sign xs) (Trafo3.fourier sign ys))

prop> QC.forAll genCyclicArray3 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo3.fourier sign xs) (Trafo3.fourier sign ys)) <= floatTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys
prop> QC.forAll genCyclicArray3 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo3.fourier sign xs) (Trafo3.fourier sign ys)) <= doubleTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys

prop> \sign -> QC.forAll genCyclicArray3 $ immutable (Trafo3.fourier sign) . arrayComplexFloat
prop> \sign -> QC.forAll genCyclicArray3 $ immutable (Trafo3.fourier sign) . arrayComplexDouble
-}
fourier ::
   (Integral n0, Integral n1, Integral n2, Class.Real a) =>
   Sign ->
   Array (Shape.Cyclic n0, Shape.Cyclic n1, Shape.Cyclic n2) (Complex a) ->
   Array (Shape.Cyclic n0, Shape.Cyclic n1, Shape.Cyclic n2) (Complex a)
fourier sign (Array sh@(Shape.Cyclic n0, Shape.Cyclic n1, Shape.Cyclic n2) x) =
   Array.unsafeCreateWithSize sh $ \n yPtr ->
   withForeignPtr x $ \xPtr ->
   when (n>0) $ run $
      FFI.planDFT3d (fromIntegral n0) (fromIntegral n1) (fromIntegral n2)
         xPtr yPtr (ffiSign sign) (FFI.estimate <> FFI.preserveInput)

{- |
prop> QC.forAll genCyclicArray3 $ \xs -> approxReal floatTol (adjust xs) (Trafo3.fourierCR (Trafo3.fourierRC xs))
prop> QC.forAll genCyclicArray3 $ \xs -> approxReal doubleTol (adjust xs) (Trafo3.fourierCR (Trafo3.fourierRC xs))

prop> QC.forAll genCyclicArray3 $ immutable Trafo3.fourierRC . arrayFloat
prop> QC.forAll genCyclicArray3 $ immutable Trafo3.fourierRC . arrayDouble
-}
fourierRC ::
   (Integral n0, Integral n1, Integral n2, Class.Real a) =>
   Array (Shape.Cyclic n0, Shape.Cyclic n1, Shape.Cyclic n2) a ->
   Array (Shape.Cyclic n0, Shape.Cyclic n1, Spectrum.Half n2) (Complex a)
fourierRC
   (Array (sh0@(Shape.Cyclic n0), sh1@(Shape.Cyclic n1), Shape.Cyclic n2) x) =

   Array.unsafeCreate (sh0, sh1, Spectrum.Half n2) $ \yPtr ->
   withForeignPtr x $ \xPtr ->
   if n0<=0 || n1<=0 || n2<=0
      then pokeArray yPtr $ replicate (fromIntegral n0 * fromIntegral n1) 0
      else run $
         FFI.planDFTr2c3d (fromIntegral n0) (fromIntegral n1) (fromIntegral n2)
            xPtr yPtr (FFI.estimate <> FFI.preserveInput)

{- |
prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable Trafo3.fourierCR . arrayComplexFloat
prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable Trafo3.fourierCR . arrayComplexDouble
-}
fourierCR ::
   (Integral n0, Integral n1, Integral n2, Class.Real a) =>
   Array (Shape.Cyclic n0, Shape.Cyclic n1, Spectrum.Half n2) (Complex a) ->
   Array (Shape.Cyclic n0, Shape.Cyclic n1, Shape.Cyclic n2) a
fourierCR
   arr@(Array
      (sh0@(Shape.Cyclic n0), sh1@(Shape.Cyclic n1), Spectrum.Half n2)
      _x) =

   Array.unsafeCreate (sh0, sh1, Shape.Cyclic n2) $ \yPtr ->
   when (n0>0 && n1>0 && n2>0) $
   runCopiedArray arr $ \xPtr ->
      FFI.planDFTc2r3d (fromIntegral n0) (fromIntegral n1) (fromIntegral n2)
         xPtr yPtr (FFI.estimate <> FFI.destroyInput)
