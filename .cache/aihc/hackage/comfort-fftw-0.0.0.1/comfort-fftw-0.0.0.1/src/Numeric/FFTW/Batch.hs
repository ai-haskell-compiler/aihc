module Numeric.FFTW.Batch (
   fourier, Sign(..), flipSign,
   fourierRC,
   fourierCR,
   ) where

import qualified Numeric.FFTW.Shape as Spectrum
import qualified Numeric.FFTW.FFI as FFI
import qualified Numeric.Netlib.Class as Class
import Numeric.FFTW.Private
         (Sign(..), flipSign, ffiSign, run, runCopiedArray, withDims)

import Foreign.Marshal.Array (pokeArray)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (nullPtr)

import qualified Data.Array.Comfort.Storable.Unchecked as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Unchecked (Array(Array))

import qualified Data.List as List
import Data.Complex (Complex)
import Data.Monoid ((<>))

import Control.Monad (when)


{- $setup
>>> import Test.Numeric.FFTW.Common
>>>    (approxReal, approxComplex, floatTol, doubleTol,
>>>     genCyclicArray2, genCyclicArray3, immutable,
>>>     arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble,
>>>     floatList, complexFloatList)
>>>
>>> import qualified Numeric.FFTW.Batch as Batch
>>> import qualified Numeric.FFTW.Rank1 as Trafo1
>>> import qualified Numeric.FFTW.Rank2 as Trafo2
>>> import qualified Numeric.FFTW.Rank3 as Trafo3
>>> import qualified Numeric.FFTW.Shape as Spectrum
>>>
>>> import qualified Numeric.Netlib.Class as Class
>>>
>>> import qualified Data.Array.Comfort.Boxed as BoxedArray
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import qualified Data.Foldable as Fold
>>> import Data.Array.Comfort.Storable (Array)
>>> import Data.Complex (Complex)
>>>
>>> import qualified Test.QuickCheck as QC
>>>
>>> array1s :: (Shape.C sh0, Shape.C sh1) =>
>>>    Array (sh0,sh1) a -> Array (sh0,(),sh1) a
>>> array1s = Array.mapShape (\(sh0,sh1) -> (sh0,(),sh1))
>>>
>>> unarray1s :: (Shape.C sh0, Shape.C sh1) =>
>>>    Array (sh0,(),sh1) a -> Array (sh0,sh1) a
>>> unarray1s = Array.mapShape (\(sh0,(),sh1) -> (sh0,sh1))
>>>
>>> array2s :: (Shape.C sh0, Shape.C sh1, Shape.C sh2) =>
>>>    Array (sh0,sh1,sh2) a -> Array (sh0,(sh1,sh2)) a
>>> array2s = Array.mapShape (\(sh0,sh1,sh2) -> (sh0,(sh1,sh2)))
>>>
>>> allApproxReal ::
>>>    (Shape.C sh0, Shape.C sh1, Class.Real a, Eq sh0, Eq sh1) =>
>>>    a ->
>>>    BoxedArray.Array sh0 (Array sh1 a) ->
>>>    BoxedArray.Array sh0 (Array sh1 a) ->
>>>    Bool
>>> allApproxReal tol xs ys =
>>>    Fold.and $ BoxedArray.zipWith (approxReal tol) xs ys
>>>
>>> allApproxComplex ::
>>>    (Shape.C sh0, Shape.C sh1, Class.Real a, Eq sh0, Eq sh1) =>
>>>    a ->
>>>    BoxedArray.Array sh0 (Array sh1 (Complex a)) ->
>>>    BoxedArray.Array sh0 (Array sh1 (Complex a)) ->
>>>    Bool
>>> allApproxComplex tol xs ys =
>>>    Fold.and $ BoxedArray.zipWith (approxComplex tol) xs ys
-}


{- |
>>> complexFloatList $ Batch.fourier Batch.Forward $ Array.fromList (Shape.ZeroBased (5::Int), Shape.Cyclic (5::Int)) [1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1]
[1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95,1.00:+0.00,(-0.81):+(-0.59),0.31:+0.95,0.31:+(-0.95),(-0.81):+0.59,1.00:+0.00,(-0.81):+0.59,0.31:+(-0.95),0.31:+0.95,(-0.81):+(-0.59),1.00:+0.00,0.31:+0.95,(-0.81):+0.59,(-0.81):+(-0.59),0.31:+(-0.95)]
>>> complexFloatList $ Batch.fourier Batch.Backward $ Array.fromList (Shape.ZeroBased (5::Int), Shape.Cyclic (5::Int)) [1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1]
[1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,0.31:+0.95,(-0.81):+0.59,(-0.81):+(-0.59),0.31:+(-0.95),1.00:+0.00,(-0.81):+0.59,0.31:+(-0.95),0.31:+0.95,(-0.81):+(-0.59),1.00:+0.00,(-0.81):+(-0.59),0.31:+0.95,0.31:+(-0.95),(-0.81):+0.59,1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95]

prop> QC.forAll genCyclicArray2 $ \xs sign -> allApproxComplex floatTol (Array.toRowArray $ Batch.fourier sign xs) (fmap (Trafo1.fourier sign) $ Array.toRowArray xs)
prop> QC.forAll genCyclicArray2 $ \xs sign -> allApproxComplex doubleTol (Array.toRowArray $ Batch.fourier sign xs) (fmap (Trafo1.fourier sign) $ Array.toRowArray xs)

prop> QC.forAll (fmap array2s genCyclicArray3) $ \xs sign -> allApproxComplex floatTol (Array.toRowArray $ Batch.fourier sign xs) (fmap (Trafo2.fourier sign) $ Array.toRowArray xs)
prop> QC.forAll (fmap array2s genCyclicArray3) $ \xs sign -> allApproxComplex doubleTol (Array.toRowArray $ Batch.fourier sign xs) (fmap (Trafo2.fourier sign) $ Array.toRowArray xs)

prop> \sign -> QC.forAll genCyclicArray2 $ immutable (Batch.fourier sign) . arrayComplexFloat
prop> \sign -> QC.forAll genCyclicArray2 $ immutable (Batch.fourier sign) . arrayComplexDouble

prop> \sign -> QC.forAll genCyclicArray3 $ immutable (Batch.fourier sign) . array2s . arrayComplexFloat
prop> \sign -> QC.forAll genCyclicArray3 $ immutable (Batch.fourier sign) . array2s . arrayComplexDouble
-}
fourier ::
   (Shape.C loop, Spectrum.MultiCyclic sh, Class.Real a) =>
   Sign -> Array (loop,sh) (Complex a) -> Array (loop,sh) (Complex a)
fourier sign (Array (loop,sh) x) =
   Array.unsafeCreateWithSize (loop,sh) $ \n yPtr ->
   withForeignPtr x $ \xPtr ->
   when (n>0) $ run $
   let sliceSize = fromIntegral $ Shape.size sh in
   withDims (Spectrum.cyclicDimensions sh) $ \rank dimPtr ->
      FFI.planManyDFT rank dimPtr (fromIntegral $ Shape.size loop)
         xPtr nullPtr 1 sliceSize
         yPtr nullPtr 1 sliceSize
         (ffiSign sign) (FFI.estimate <> FFI.preserveInput)

{- |
prop> QC.forAll genCyclicArray2 $ \xs -> allApproxComplex floatTol (Array.toRowArray $ unarray1s $ Batch.fourierRC $ array1s xs) (fmap Trafo1.fourierRC $ Array.toRowArray xs)
prop> QC.forAll genCyclicArray2 $ \xs -> allApproxComplex doubleTol (Array.toRowArray $ unarray1s $ Batch.fourierRC $ array1s xs) (fmap Trafo1.fourierRC $ Array.toRowArray xs)

prop> QC.forAll genCyclicArray3 $ \xs -> allApproxComplex floatTol (Array.toRowArray $ array2s $ Batch.fourierRC xs) (fmap Trafo2.fourierRC $ Array.toRowArray $ array2s xs)
prop> QC.forAll genCyclicArray3 $ \xs -> allApproxComplex doubleTol (Array.toRowArray $ array2s $ Batch.fourierRC xs) (fmap Trafo2.fourierRC $ Array.toRowArray $ array2s xs)

prop> QC.forAll genCyclicArray2 $ immutable Batch.fourierRC . array1s . arrayFloat
prop> QC.forAll genCyclicArray2 $ immutable Batch.fourierRC . array1s . arrayDouble

prop> QC.forAll genCyclicArray3 $ immutable Batch.fourierRC . arrayFloat
prop> QC.forAll genCyclicArray3 $ immutable Batch.fourierRC . arrayDouble
-}
fourierRC ::
   (Shape.C loop, Spectrum.MultiCyclic sh0, Integral n1, Class.Real a) =>
   Array (loop, sh0, Shape.Cyclic n1) a ->
   Array (loop, sh0, Spectrum.Half n1) (Complex a)
fourierRC (Array (loop, sh0, sh1x@(Shape.Cyclic n1)) x) =
   let sh1y = Spectrum.Half n1 in
   Array.unsafeCreate (loop, sh0, sh1y) $ \yPtr ->
   withForeignPtr x $ \xPtr ->
   let dims = Spectrum.cyclicDimensions (sh0, sh1x) in
   if any (<=0) dims
      then pokeArray yPtr $ List.genericReplicate (Shape.size (loop,sh0)) 0
      else run $
         withDims dims $ \rank dimPtr ->
         FFI.planManyDFTr2c rank dimPtr (fromIntegral $ Shape.size loop)
            xPtr nullPtr 1 (fromIntegral $ Shape.size (sh0, sh1x))
            yPtr nullPtr 1 (fromIntegral $ Shape.size (sh0, sh1y))
            (FFI.estimate <> FFI.preserveInput)

{- |
>>> floatList $ Batch.fourierCR $ Array.fromList (Shape.ZeroBased (2::Int), (), Spectrum.Half (3::Int)) [1,0, 0,-1]
[1.00,1.00,1.00,-2.00,1.00,1.00]

>>> floatList $ Batch.fourierCR $ Array.fromList ((), Shape.Cyclic (3::Int), Spectrum.Half (1::Int)) [1,0,0]
[1.00,1.00,1.00]
>>> floatList $ Batch.fourierCR $ Array.fromList ((), Shape.Cyclic (3::Int), Spectrum.Half (1::Int)) [1,0,0]
[1.00,1.00,1.00]

prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ \xs -> allApproxReal floatTol (Array.toRowArray $ unarray1s $ Batch.fourierCR $ array1s xs) (fmap Trafo1.fourierCR $ Array.toRowArray xs)
prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ \xs -> allApproxReal doubleTol (Array.toRowArray $ unarray1s $ Batch.fourierCR $ array1s xs) (fmap Trafo1.fourierCR $ Array.toRowArray xs)

prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ \xs -> allApproxReal floatTol (Array.toRowArray $ array2s $ Batch.fourierCR xs) (fmap Trafo2.fourierCR $ Array.toRowArray $ array2s xs)
prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ \xs -> allApproxReal doubleTol (Array.toRowArray $ array2s $ Batch.fourierCR xs) (fmap Trafo2.fourierCR $ Array.toRowArray $ array2s xs)


prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable Batch.fourierCR . array1s . arrayComplexFloat
prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable Batch.fourierCR . array1s . arrayComplexDouble

prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable Batch.fourierCR . arrayComplexFloat
prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable Batch.fourierCR . arrayComplexDouble
-}
fourierCR ::
   (Shape.C loop, Spectrum.MultiCyclic sh0, Integral n1, Class.Real a) =>
   Array (loop, sh0, Spectrum.Half n1) (Complex a) ->
   Array (loop, sh0, Shape.Cyclic n1) a
fourierCR arr@(Array (loop, sh0, sh1x@(Spectrum.Half n1)) _x) =
   let sh1y = Shape.Cyclic n1 in
   let sh = (sh0, sh1y) in
   let dims = Spectrum.cyclicDimensions sh in
   Array.unsafeCreate (loop, sh0, sh1y) $ \yPtr ->
   when (all (>0) dims) $
   runCopiedArray arr $ \xPtr ->
   withDims dims $ \rank dimPtr ->
      FFI.planManyDFTc2r rank dimPtr (fromIntegral $ Shape.size loop)
         xPtr nullPtr 1 (fromIntegral $ Shape.size (sh0, sh1x))
         yPtr nullPtr 1 (fromIntegral $ Shape.size (sh0, sh1y))
         (FFI.estimate <> FFI.destroyInput)
