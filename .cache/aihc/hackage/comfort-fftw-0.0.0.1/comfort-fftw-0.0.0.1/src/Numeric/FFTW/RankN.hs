module Numeric.FFTW.RankN (
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
>>>     genCyclicArray1, genCyclicArray2, genCyclicArray3, immutable,
>>>     arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble,
>>>     floatList, complexFloatList)
>>>
>>> import qualified Numeric.FFTW.RankN as TrafoM
>>> import qualified Numeric.FFTW.Rank1 as Trafo1
>>> import qualified Numeric.FFTW.Rank2 as Trafo2
>>> import qualified Numeric.FFTW.Rank3 as Trafo3
>>> import qualified Numeric.FFTW.Shape as Spectrum
>>>
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import Data.Array.Comfort.Storable (Array)
>>>
>>> import qualified Test.QuickCheck as QC
>>>
>>> array1 :: (Shape.C sh) => Array sh a -> Array ((), sh) a
>>> array1 = Array.mapShape ((,) ())
>>>
>>> array3 :: (Shape.C sh0, Shape.C sh1, Shape.C sh2) =>
>>>    Array (sh0,sh1,sh2) a -> Array ((sh0,sh1),sh2) a
>>> array3 = Array.mapShape (\(sh0,sh1,sh2) -> ((sh0,sh1),sh2))
-}


{- |
>>> complexFloatList $ TrafoM.fourier TrafoM.Forward $ Array.fromList (Shape.Cyclic (5::Int), Shape.Cyclic (5::Int)) [0,0,0,0,0, 0,1,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0]
[1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95,1.00:+0.00,(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95,1.00:+0.00,0.31:+(-0.95),(-0.81):+0.59,0.31:+0.95,1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),0.31:+0.95,1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59]

prop> QC.forAll genCyclicArray1 $ \xs sign -> approxComplex floatTol (array1 $ Trafo1.fourier sign xs) (TrafoM.fourier sign $ array1 xs)
prop> QC.forAll genCyclicArray1 $ \xs sign -> approxComplex doubleTol (array1 $ Trafo1.fourier sign xs) (TrafoM.fourier sign $ array1 xs)

prop> QC.forAll genCyclicArray2 $ \xs sign -> approxComplex floatTol (Trafo2.fourier sign xs) (TrafoM.fourier sign xs)
prop> QC.forAll genCyclicArray2 $ \xs sign -> approxComplex doubleTol (Trafo2.fourier sign xs) (TrafoM.fourier sign xs)

prop> QC.forAll genCyclicArray3 $ \xs sign -> approxComplex floatTol (array3 $ Trafo3.fourier sign xs) (TrafoM.fourier sign $ array3 xs)
prop> QC.forAll genCyclicArray3 $ \xs sign -> approxComplex doubleTol (array3 $ Trafo3.fourier sign xs) (TrafoM.fourier sign $ array3 xs)
-}
fourier ::
   (Spectrum.MultiCyclic sh, Class.Real a) =>
   Sign -> Array sh (Complex a) -> Array sh (Complex a)
fourier sign (Array sh x) =
   Array.unsafeCreateWithSize sh $ \n yPtr ->
   withForeignPtr x $ \xPtr ->
   when (n>0) $ run $
   withDims (Spectrum.cyclicDimensions sh) $ \rank dimPtr ->
      FFI.planDFT rank dimPtr xPtr yPtr (ffiSign sign)
         (FFI.estimate <> FFI.preserveInput)

{- |
prop> QC.forAll genCyclicArray1 $ \xs -> approxComplex floatTol (array1 $ Trafo1.fourierRC xs) (TrafoM.fourierRC $ array1 xs)
prop> QC.forAll genCyclicArray1 $ \xs -> approxComplex doubleTol (array1 $ Trafo1.fourierRC xs) (TrafoM.fourierRC $ array1 xs)

prop> QC.forAll genCyclicArray2 $ \xs -> approxComplex floatTol (Trafo2.fourierRC xs) (TrafoM.fourierRC xs)
prop> QC.forAll genCyclicArray2 $ \xs -> approxComplex doubleTol (Trafo2.fourierRC xs) (TrafoM.fourierRC xs)

prop> QC.forAll genCyclicArray3 $ \xs -> approxComplex floatTol (array3 $ Trafo3.fourierRC xs) (TrafoM.fourierRC $ array3 xs)
prop> QC.forAll genCyclicArray3 $ \xs -> approxComplex doubleTol (array3 $ Trafo3.fourierRC xs) (TrafoM.fourierRC $ array3 xs)


prop> QC.forAll genCyclicArray1 $ immutable TrafoM.fourierRC . array1 . arrayFloat
prop> QC.forAll genCyclicArray1 $ immutable TrafoM.fourierRC . array1 . arrayDouble

prop> QC.forAll genCyclicArray2 $ immutable TrafoM.fourierRC . arrayFloat
prop> QC.forAll genCyclicArray2 $ immutable TrafoM.fourierRC . arrayDouble

prop> QC.forAll genCyclicArray3 $ immutable TrafoM.fourierRC . array3 . arrayFloat
prop> QC.forAll genCyclicArray3 $ immutable TrafoM.fourierRC . array3 . arrayDouble
-}
fourierRC ::
   (Spectrum.MultiCyclic sh0, Integral n1, Class.Real a) =>
   Array (sh0, Shape.Cyclic n1) a ->
   Array (sh0, Spectrum.Half n1) (Complex a)
fourierRC (Array sh@(sh0, Shape.Cyclic n1) x) =
   Array.unsafeCreate (sh0, Spectrum.Half n1) $ \yPtr ->
   withForeignPtr x $ \xPtr ->
   let dims = Spectrum.cyclicDimensions sh in
   if any (<=0) dims
      then pokeArray yPtr $ List.genericReplicate (Shape.size sh0) 0
      else run $
         withDims dims $ \rank dimPtr ->
         FFI.planDFTr2c rank dimPtr xPtr yPtr
            (FFI.estimate <> FFI.preserveInput)

{- |
>>> floatList $ Trafo1.fourierCR $ Array.fromList (Spectrum.Half (3::Int)) [1,0 ]
[1.00,1.00,1.00]
>>> floatList $ TrafoM.fourierCR $ Array.fromList ((), Spectrum.Half (3::Int)) [1,0]
[1.00,1.00,1.00]

>>> floatList $ Trafo2.fourierCR $ Array.fromList (Shape.Cyclic (3::Int), Spectrum.Half (1::Int)) [1,0,0]
[1.00,1.00,1.00]
>>> floatList $ TrafoM.fourierCR $ Array.fromList (Shape.Cyclic (3::Int), Spectrum.Half (1::Int)) [1,0,0]
[1.00,1.00,1.00]

prop> QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ \xs -> approxReal floatTol (array1 $ Trafo1.fourierCR xs) (TrafoM.fourierCR $ array1 xs)
prop> QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ \xs -> approxReal doubleTol (array1 $ Trafo1.fourierCR xs) (TrafoM.fourierCR $ array1 xs)

prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ \xs -> approxReal floatTol (Trafo2.fourierCR xs) (TrafoM.fourierCR xs)
prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ \xs -> approxReal doubleTol (Trafo2.fourierCR xs) (TrafoM.fourierCR xs)

prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ \xs -> approxReal floatTol (array3 $ Trafo3.fourierCR xs) (TrafoM.fourierCR $ array3 xs)
prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ \xs -> approxReal doubleTol (array3 $ Trafo3.fourierCR xs) (TrafoM.fourierCR $ array3 xs)


prop> QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ immutable TrafoM.fourierCR . array1 . arrayComplexFloat
prop> QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ immutable TrafoM.fourierCR . array1 . arrayComplexDouble

prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable TrafoM.fourierCR . arrayComplexFloat
prop> QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable TrafoM.fourierCR . arrayComplexDouble

prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable TrafoM.fourierCR . array3 . arrayComplexFloat
prop> QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable TrafoM.fourierCR . array3 . arrayComplexDouble
-}
fourierCR ::
   (Spectrum.MultiCyclic sh0, Integral n1, Class.Real a) =>
   Array (sh0, Spectrum.Half n1) (Complex a) ->
   Array (sh0, Shape.Cyclic n1) a
fourierCR arr@(Array (sh0, Spectrum.Half n1) _x) =
   let sh = (sh0, Shape.Cyclic n1) in
   let dims = Spectrum.cyclicDimensions sh in
   Array.unsafeCreate sh $ \yPtr ->
   when (all (>0) dims) $
   runCopiedArray arr $ \xPtr ->
   withDims dims $ \rank dimPtr ->
      FFI.planDFTc2r rank dimPtr xPtr yPtr (FFI.estimate <> FFI.destroyInput)
