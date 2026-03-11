{-# LANGUAGE GADTs #-}
module Numeric.FFTW.Rank1 (
   fourier, Sign(..), flipSign,
   fourierRC,
   fourierCR,
   cosine,
   sine,
   hartley,
   ) where

import qualified Numeric.FFTW.Shape as Spectrum
import qualified Numeric.FFTW.FFI as FFI
import qualified Numeric.Netlib.Class as Class
import Numeric.FFTW.Private (Sign(..), flipSign, ffiSign, run)

import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (poke)

import qualified Data.Array.Comfort.Storable.Unchecked as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Unchecked (Array(Array))

import Data.Complex (Complex)
import Data.Monoid ((<>))

import Control.Monad (when)


{- $setup
>>> :set -XGADTs
>>> import Test.Numeric.FFTW.Common
>>>    (approxReal, approxComplex, floatTol, doubleTol, normInf,
>>>     adjust, scalarProduct, split, genCyclicArray1, immutable,
>>>     arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble,
>>>     floatList, complexFloatList)
>>>
>>> import qualified Numeric.FFTW.Rank1 as Trafo1
>>> import qualified Numeric.FFTW.Shape as Spectrum
>>> import Numeric.FFTW.Rank1 (flipSign)
>>> import Numeric.FFTW.Shape
>>>    (SymmetrySingleton(Even,Odd), ShiftSingleton(Exact,Halfway))
>>>
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import qualified Data.Complex as Complex
>>> import Data.Array.Comfort.Storable (Array)
>>>
>>> import qualified Numeric.Netlib.Class as Class
>>>
>>> import Foreign.Storable (Storable)
>>>
>>> import qualified Test.QuickCheck as QC
>>>
>>> genSymmetric ::
>>>    (Spectrum.Symmetry symmetry) =>
>>>    (Spectrum.Shift shiftTime) =>
>>>    (Spectrum.Shift shiftSpectrum) =>
>>>    (Spectrum.Symmetric symmetry shiftTime shiftSpectrum Int ~ sh) =>
>>>    (QC.Arbitrary a, Storable a) =>
>>>    Spectrum.SymmetrySingleton symmetry ->
>>>    Spectrum.ShiftSingleton shiftTime ->
>>>    Spectrum.ShiftSingleton shiftSpectrum ->
>>>    QC.Gen (Array sh a)
>>> genSymmetric symmetry shiftTime shiftSpectrum = do
>>>    xs <- fmap (take 1000) $ QC.arbitrary
>>>    return $ Array.fromList
>>>       (Spectrum.Symmetric symmetry shiftTime shiftSpectrum $ length xs) xs
>>>
>>> adjustSymmetric ::
>>>    (Spectrum.Symmetry symmetry) =>
>>>    (Spectrum.Shift shiftTime) =>
>>>    (Spectrum.Shift shiftSpectrum) =>
>>>    (Spectrum.Symmetric symmetry shiftTime shiftSpectrum Int ~ sh) =>
>>>    (Class.Floating a) =>
>>>    Array sh a -> Array sh a
>>> adjustSymmetric xs =
>>>    let n = Spectrum.symmetricLogicalSize (Array.shape xs)
>>>    in Array.map (fromIntegral n *) xs
-}


{- |
>>> complexFloatList $ Trafo1.fourier Trafo1.Forward $ Array.fromList (Shape.Cyclic (5::Int)) [1,0,0,0,0]
[1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00]
>>> complexFloatList $ Trafo1.fourier Trafo1.Forward $ Array.fromList (Shape.Cyclic (5::Int)) [0,1,0,0,0]
 [1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95]
>>> complexFloatList $ Trafo1.fourier Trafo1.Backward $ Array.fromList (Shape.Cyclic (5::Int)) [0,1,0,0,0]
[1.00:+0.00,0.31:+0.95,(-0.81):+0.59,(-0.81):+(-0.59),0.31:+(-0.95)]

prop> QC.forAll genCyclicArray1 $ \xs sign-> approxComplex floatTol (adjust xs) (Trafo1.fourier sign (Trafo1.fourier (flipSign sign) xs))
prop> QC.forAll genCyclicArray1 $ \xs sign -> approxComplex doubleTol (adjust xs) (Trafo1.fourier sign (Trafo1.fourier (flipSign sign) xs))

prop> QC.forAll genCyclicArray1 $ \xs sign -> approxComplex floatTol (Trafo1.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo1.fourier (flipSign sign) xs))
prop> QC.forAll genCyclicArray1 $ \xs sign -> approxComplex doubleTol (Trafo1.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo1.fourier (flipSign sign) xs))

prop> QC.forAll genCyclicArray1 $ \xys sign -> let (xs,ys) = split xys in approxComplex floatTol (Trafo1.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo1.fourier sign xs) (Trafo1.fourier sign ys))
prop> QC.forAll genCyclicArray1 $ \xys sign -> let (xs,ys) = split xys in approxComplex doubleTol (Trafo1.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo1.fourier sign xs) (Trafo1.fourier sign ys))

prop> QC.forAll genCyclicArray1 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo1.fourier sign xs) (Trafo1.fourier sign ys)) <= floatTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys
prop> QC.forAll genCyclicArray1 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo1.fourier sign xs) (Trafo1.fourier sign ys)) <= doubleTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys

prop> \sign -> QC.forAll genCyclicArray1 $ immutable (Trafo1.fourier sign) . arrayComplexFloat
prop> \sign -> QC.forAll genCyclicArray1 $ immutable (Trafo1.fourier sign) . arrayComplexDouble
-}
fourier ::
   (Integral n, Class.Real a) =>
   Sign ->
   Array (Shape.Cyclic n) (Complex a) ->
   Array (Shape.Cyclic n) (Complex a)
fourier sign (Array sh x) =
   Array.unsafeCreateWithSize sh $ \n yPtr ->
   withForeignPtr x $ \xPtr ->
   when (n>0) $
   run $ FFI.planDFT1d (fromIntegral n) xPtr yPtr (ffiSign sign)
            (FFI.estimate <> FFI.preserveInput)

{- |
prop> QC.forAll genCyclicArray1 $ \xs -> approxReal floatTol (adjust xs) (Trafo1.fourierCR (Trafo1.fourierRC xs))
prop> QC.forAll genCyclicArray1 $ \xs -> approxReal doubleTol (adjust xs) (Trafo1.fourierCR (Trafo1.fourierRC xs))

prop> QC.forAll genCyclicArray1 $ immutable Trafo1.fourierRC . arrayFloat
prop> QC.forAll genCyclicArray1 $ immutable Trafo1.fourierRC . arrayDouble
-}
fourierRC ::
   (Integral n, Class.Real a) =>
   Array (Shape.Cyclic n) a ->
   Array (Spectrum.Half n) (Complex a)
fourierRC (Array (Shape.Cyclic n) x) =
   Array.unsafeCreate (Spectrum.Half n) $ \yPtr ->
   withForeignPtr x $ \xPtr ->
   if n<=0
      then poke yPtr 0
      else run $ FFI.planDFTr2c1d (fromIntegral n) xPtr yPtr
                     (FFI.estimate <> FFI.preserveInput)

{- |
>>> floatList $ Trafo1.fourierCR $ Array.fromList (Spectrum.Half (5::Int)) [0,1,0]
[2.00,0.62,-1.62,-1.62,0.62]

prop> QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ immutable Trafo1.fourierCR . arrayComplexFloat
prop> QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ immutable Trafo1.fourierCR . arrayComplexDouble
-}
fourierCR ::
   (Integral n, Class.Real a) =>
   Array (Spectrum.Half n) (Complex a) ->
   Array (Shape.Cyclic n) a
fourierCR (Array (Spectrum.Half n) x) =
   Array.unsafeCreate (Shape.Cyclic n) $ \yPtr ->
   withForeignPtr x $ \xPtr ->
   when (n>0) $
   run $ FFI.planDFTc2r1d (fromIntegral n) xPtr yPtr
            (FFI.estimate <> FFI.preserveInput)


{- |
@Symmetric Even Halfway Exact@ yields _the_ DCT,
@Symmetric Even Exact Halfway@ yields _the_ inverse DCT.

prop> QC.forAll (genSymmetric Even Exact Exact `QC.suchThat` ((>=2) . Shape.size . Array.shape)) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs))
prop> QC.forAll (genSymmetric Even Exact Exact `QC.suchThat` ((>=2) . Shape.size . Array.shape)) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs))
prop> QC.forAll (genSymmetric Even Halfway Exact) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs))
prop> QC.forAll (genSymmetric Even Halfway Exact) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs))
prop> QC.forAll (genSymmetric Even Exact Halfway) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs))
prop> QC.forAll (genSymmetric Even Exact Halfway) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs))
prop> QC.forAll (genSymmetric Even Halfway Halfway) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs))
prop> QC.forAll (genSymmetric Even Halfway Halfway) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs))

prop> QC.forAll (genSymmetric Even Exact Exact `QC.suchThat` ((>=2) . Shape.size . Array.shape)) $ immutable Trafo1.cosine . arrayFloat
prop> QC.forAll (genSymmetric Even Exact Exact `QC.suchThat` ((>=2) . Shape.size . Array.shape)) $ immutable Trafo1.cosine . arrayDouble
prop> QC.forAll (genSymmetric Even Halfway Exact) $ immutable Trafo1.cosine . arrayFloat
prop> QC.forAll (genSymmetric Even Halfway Exact) $ immutable Trafo1.cosine . arrayDouble
prop> QC.forAll (genSymmetric Even Exact Halfway) $ immutable Trafo1.cosine . arrayFloat
prop> QC.forAll (genSymmetric Even Exact Halfway) $ immutable Trafo1.cosine . arrayDouble
prop> QC.forAll (genSymmetric Even Halfway Halfway) $ immutable Trafo1.cosine . arrayFloat
prop> QC.forAll (genSymmetric Even Halfway Halfway) $ immutable Trafo1.cosine . arrayDouble
-}
cosine ::
   (Spectrum.Shift shiftTime, Spectrum.Shift shiftSpectrum,
    Integral n, Class.Real a) =>
   Array (Spectrum.Symmetric Spectrum.Even shiftTime shiftSpectrum n) a ->
   Array (Spectrum.Symmetric Spectrum.Even shiftSpectrum shiftTime n) a
cosine (Array (Spectrum.Symmetric symmetry shiftTime shiftSpectrum n) x) =
   Array.unsafeCreate
      (Spectrum.Symmetric symmetry shiftSpectrum shiftTime n) $ \yPtr ->
   withForeignPtr x $ \xPtr ->
   when (n>0) $
   let kind =
         case (shiftTime,shiftSpectrum) of
            (Spectrum.Halfway, Spectrum.Exact  ) -> FFI.redft10
            (Spectrum.Exact,   Spectrum.Halfway) -> FFI.redft01
            (Spectrum.Halfway, Spectrum.Halfway) -> FFI.redft11
            (Spectrum.Exact,   Spectrum.Exact  ) ->
               if n>1
                  then FFI.redft00
                  else error "DCT-1 must have at least two input data Exacts"
   in run $ FFI.planR2r1d (fromIntegral n) xPtr yPtr kind
               (FFI.estimate <> FFI.preserveInput)

{- |
prop> QC.forAll (genSymmetric Odd Exact Exact) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs))
prop> QC.forAll (genSymmetric Odd Exact Exact) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs))
prop> QC.forAll (genSymmetric Odd Halfway Exact) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs))
prop> QC.forAll (genSymmetric Odd Halfway Exact) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs))
prop> QC.forAll (genSymmetric Odd Exact Halfway) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs))
prop> QC.forAll (genSymmetric Odd Exact Halfway) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs))
prop> QC.forAll (genSymmetric Odd Halfway Halfway) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs))
prop> QC.forAll (genSymmetric Odd Halfway Halfway) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs))

prop> QC.forAll (genSymmetric Odd Exact Exact) $ immutable Trafo1.sine . arrayFloat
prop> QC.forAll (genSymmetric Odd Exact Exact) $ immutable Trafo1.sine . arrayDouble
prop> QC.forAll (genSymmetric Odd Halfway Exact) $ immutable Trafo1.sine . arrayFloat
prop> QC.forAll (genSymmetric Odd Halfway Exact) $ immutable Trafo1.sine . arrayDouble
prop> QC.forAll (genSymmetric Odd Exact Halfway) $ immutable Trafo1.sine . arrayFloat
prop> QC.forAll (genSymmetric Odd Exact Halfway) $ immutable Trafo1.sine . arrayDouble
prop> QC.forAll (genSymmetric Odd Halfway Halfway) $ immutable Trafo1.sine . arrayFloat
prop> QC.forAll (genSymmetric Odd Halfway Halfway) $ immutable Trafo1.sine . arrayDouble
-}
sine ::
   (Spectrum.Shift shiftTime, Spectrum.Shift shiftSpectrum,
    Integral n, Class.Real a) =>
   Array (Spectrum.Symmetric Spectrum.Odd shiftTime shiftSpectrum n) a ->
   Array (Spectrum.Symmetric Spectrum.Odd shiftSpectrum shiftTime n) a
sine (Array (Spectrum.Symmetric symmetry shiftTime shiftSpectrum n) x) =
   Array.unsafeCreate
      (Spectrum.Symmetric symmetry shiftSpectrum shiftTime n) $ \yPtr ->
   withForeignPtr x $ \xPtr ->
   when (n>0) $
   let kind =
         case (shiftTime,shiftSpectrum) of
            (Spectrum.Exact,   Spectrum.Exact  ) -> FFI.rodft00
            (Spectrum.Halfway, Spectrum.Exact  ) -> FFI.rodft10
            (Spectrum.Exact,   Spectrum.Halfway) -> FFI.rodft01
            (Spectrum.Halfway, Spectrum.Halfway) -> FFI.rodft11
   in run $ FFI.planR2r1d (fromIntegral n) xPtr yPtr kind
               (FFI.estimate <> FFI.preserveInput)


{- |
prop> QC.forAll genCyclicArray1 $ \xs -> approxReal floatTol (adjust xs) (Trafo1.hartley (Trafo1.hartley xs))
prop> QC.forAll genCyclicArray1 $ \xs -> approxReal doubleTol (adjust xs) (Trafo1.hartley (Trafo1.hartley xs))

prop> QC.forAll genCyclicArray1 $ immutable Trafo1.hartley . arrayFloat
prop> QC.forAll genCyclicArray1 $ immutable Trafo1.hartley . arrayDouble
-}
hartley ::
   (Integral n, Class.Real a) =>
   Array (Shape.Cyclic n) a ->
   Array (Shape.Cyclic n) a
hartley (Array sh@(Shape.Cyclic n) x) =
   Array.unsafeCreate sh $ \yPtr ->
   withForeignPtr x $ \xPtr ->
   when (n>0) $
   run $ FFI.planR2r1d (fromIntegral n) xPtr yPtr FFI.dht
            (FFI.estimate <> FFI.preserveInput)
