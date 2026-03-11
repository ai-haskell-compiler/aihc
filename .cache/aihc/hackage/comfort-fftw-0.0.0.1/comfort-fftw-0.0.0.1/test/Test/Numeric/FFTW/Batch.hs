-- Do not edit! Automatically created with doctest-extract from src/Numeric/FFTW/Batch.hs
{-# LINE 28 "src/Numeric/FFTW/Batch.hs" #-}

module Test.Numeric.FFTW.Batch where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 29 "src/Numeric/FFTW/Batch.hs" #-}
import     Test.Numeric.FFTW.Common
       (approxReal, approxComplex, floatTol, doubleTol,
        genCyclicArray2, genCyclicArray3, immutable,
        arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble,
        floatList, complexFloatList)

import     qualified Numeric.FFTW.Batch as Batch
import     qualified Numeric.FFTW.Rank1 as Trafo1
import     qualified Numeric.FFTW.Rank2 as Trafo2
import     qualified Numeric.FFTW.Rank3 as Trafo3
import     qualified Numeric.FFTW.Shape as Spectrum

import     qualified Numeric.Netlib.Class as Class

import     qualified Data.Array.Comfort.Boxed as BoxedArray
import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     qualified Data.Foldable as Fold
import     Data.Array.Comfort.Storable (Array)
import     Data.Complex (Complex)

import     qualified Test.QuickCheck as QC

array1s     :: (Shape.C sh0, Shape.C sh1) =>
       Array (sh0,sh1) a -> Array (sh0,(),sh1) a
array1s     = Array.mapShape (\(sh0,sh1) -> (sh0,(),sh1))

unarray1s     :: (Shape.C sh0, Shape.C sh1) =>
       Array (sh0,(),sh1) a -> Array (sh0,sh1) a
unarray1s     = Array.mapShape (\(sh0,(),sh1) -> (sh0,sh1))

array2s     :: (Shape.C sh0, Shape.C sh1, Shape.C sh2) =>
       Array (sh0,sh1,sh2) a -> Array (sh0,(sh1,sh2)) a
array2s     = Array.mapShape (\(sh0,sh1,sh2) -> (sh0,(sh1,sh2)))

allApproxReal     ::
       (Shape.C sh0, Shape.C sh1, Class.Real a, Eq sh0, Eq sh1) =>
       a ->
       BoxedArray.Array sh0 (Array sh1 a) ->
       BoxedArray.Array sh0 (Array sh1 a) ->
       Bool
allApproxReal     tol xs ys =
       Fold.and $ BoxedArray.zipWith (approxReal tol) xs ys

allApproxComplex     ::
       (Shape.C sh0, Shape.C sh1, Class.Real a, Eq sh0, Eq sh1) =>
       a ->
       BoxedArray.Array sh0 (Array sh1 (Complex a)) ->
       BoxedArray.Array sh0 (Array sh1 (Complex a)) ->
       Bool
allApproxComplex     tol xs ys =
       Fold.and $ BoxedArray.zipWith (approxComplex tol) xs ys

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.FFTW.Batch:90: "
{-# LINE 90 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 90 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs sign -> allApproxComplex floatTol (Array.toRowArray $ Batch.fourier sign xs) (fmap (Trafo1.fourier sign) $ Array.toRowArray xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:91: "
{-# LINE 91 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 91 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs sign -> allApproxComplex doubleTol (Array.toRowArray $ Batch.fourier sign xs) (fmap (Trafo1.fourier sign) $ Array.toRowArray xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:93: "
{-# LINE 93 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 93 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap array2s genCyclicArray3) $ \xs sign -> allApproxComplex floatTol (Array.toRowArray $ Batch.fourier sign xs) (fmap (Trafo2.fourier sign) $ Array.toRowArray xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:94: "
{-# LINE 94 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 94 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap array2s genCyclicArray3) $ \xs sign -> allApproxComplex doubleTol (Array.toRowArray $ Batch.fourier sign xs) (fmap (Trafo2.fourier sign) $ Array.toRowArray xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:96: "
{-# LINE 96 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 96 "src/Numeric/FFTW/Batch.hs" #-}
     (\sign -> QC.forAll genCyclicArray2 $ immutable (Batch.fourier sign) . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Batch:97: "
{-# LINE 97 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 97 "src/Numeric/FFTW/Batch.hs" #-}
     (\sign -> QC.forAll genCyclicArray2 $ immutable (Batch.fourier sign) . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.Batch:99: "
{-# LINE 99 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 99 "src/Numeric/FFTW/Batch.hs" #-}
     (\sign -> QC.forAll genCyclicArray3 $ immutable (Batch.fourier sign) . array2s . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Batch:100: "
{-# LINE 100 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 100 "src/Numeric/FFTW/Batch.hs" #-}
     (\sign -> QC.forAll genCyclicArray3 $ immutable (Batch.fourier sign) . array2s . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.Batch:85: "
{-# LINE 85 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.example
{-# LINE 85 "src/Numeric/FFTW/Batch.hs" #-}
   (complexFloatList $ Batch.fourier Batch.Forward $ Array.fromList (Shape.ZeroBased (5::Int), Shape.Cyclic (5::Int)) [1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1])
  [ExpectedLine [LineChunk "[1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95,1.00:+0.00,(-0.81):+(-0.59),0.31:+0.95,0.31:+(-0.95),(-0.81):+0.59,1.00:+0.00,(-0.81):+0.59,0.31:+(-0.95),0.31:+0.95,(-0.81):+(-0.59),1.00:+0.00,0.31:+0.95,(-0.81):+0.59,(-0.81):+(-0.59),0.31:+(-0.95)]"]]
 DocTest.printPrefix "Numeric.FFTW.Batch:87: "
{-# LINE 87 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.example
{-# LINE 87 "src/Numeric/FFTW/Batch.hs" #-}
   (complexFloatList $ Batch.fourier Batch.Backward $ Array.fromList (Shape.ZeroBased (5::Int), Shape.Cyclic (5::Int)) [1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1])
  [ExpectedLine [LineChunk "[1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,0.31:+0.95,(-0.81):+0.59,(-0.81):+(-0.59),0.31:+(-0.95),1.00:+0.00,(-0.81):+0.59,0.31:+(-0.95),0.31:+0.95,(-0.81):+(-0.59),1.00:+0.00,(-0.81):+(-0.59),0.31:+0.95,0.31:+(-0.95),(-0.81):+0.59,1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95]"]]
 DocTest.printPrefix "Numeric.FFTW.Batch:117: "
{-# LINE 117 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 117 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs -> allApproxComplex floatTol (Array.toRowArray $ unarray1s $ Batch.fourierRC $ array1s xs) (fmap Trafo1.fourierRC $ Array.toRowArray xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:118: "
{-# LINE 118 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 118 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs -> allApproxComplex doubleTol (Array.toRowArray $ unarray1s $ Batch.fourierRC $ array1s xs) (fmap Trafo1.fourierRC $ Array.toRowArray xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:120: "
{-# LINE 120 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 120 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs -> allApproxComplex floatTol (Array.toRowArray $ array2s $ Batch.fourierRC xs) (fmap Trafo2.fourierRC $ Array.toRowArray $ array2s xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:121: "
{-# LINE 121 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 121 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs -> allApproxComplex doubleTol (Array.toRowArray $ array2s $ Batch.fourierRC xs) (fmap Trafo2.fourierRC $ Array.toRowArray $ array2s xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:123: "
{-# LINE 123 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 123 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray2 $ immutable Batch.fourierRC . array1s . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Batch:124: "
{-# LINE 124 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 124 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray2 $ immutable Batch.fourierRC . array1s . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Batch:126: "
{-# LINE 126 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 126 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray3 $ immutable Batch.fourierRC . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Batch:127: "
{-# LINE 127 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 127 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll genCyclicArray3 $ immutable Batch.fourierRC . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Batch:156: "
{-# LINE 156 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 156 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ \xs -> allApproxReal floatTol (Array.toRowArray $ unarray1s $ Batch.fourierCR $ array1s xs) (fmap Trafo1.fourierCR $ Array.toRowArray xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:157: "
{-# LINE 157 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 157 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ \xs -> allApproxReal doubleTol (Array.toRowArray $ unarray1s $ Batch.fourierCR $ array1s xs) (fmap Trafo1.fourierCR $ Array.toRowArray xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:159: "
{-# LINE 159 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 159 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ \xs -> allApproxReal floatTol (Array.toRowArray $ array2s $ Batch.fourierCR xs) (fmap Trafo2.fourierCR $ Array.toRowArray $ array2s xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:160: "
{-# LINE 160 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 160 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ \xs -> allApproxReal doubleTol (Array.toRowArray $ array2s $ Batch.fourierCR xs) (fmap Trafo2.fourierCR $ Array.toRowArray $ array2s xs))
 DocTest.printPrefix "Numeric.FFTW.Batch:163: "
{-# LINE 163 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 163 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable Batch.fourierCR . array1s . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Batch:164: "
{-# LINE 164 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 164 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable Batch.fourierCR . array1s . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.Batch:166: "
{-# LINE 166 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 166 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable Batch.fourierCR . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Batch:167: "
{-# LINE 167 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.property
{-# LINE 167 "src/Numeric/FFTW/Batch.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable Batch.fourierCR . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.Batch:148: "
{-# LINE 148 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.example
{-# LINE 148 "src/Numeric/FFTW/Batch.hs" #-}
   (floatList $ Batch.fourierCR $ Array.fromList (Shape.ZeroBased (2::Int), (), Spectrum.Half (3::Int)) [1,0, 0,-1])
  [ExpectedLine [LineChunk "[1.00,1.00,1.00,-2.00,1.00,1.00]"]]
 DocTest.printPrefix "Numeric.FFTW.Batch:151: "
{-# LINE 151 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.example
{-# LINE 151 "src/Numeric/FFTW/Batch.hs" #-}
   (floatList $ Batch.fourierCR $ Array.fromList ((), Shape.Cyclic (3::Int), Spectrum.Half (1::Int)) [1,0,0])
  [ExpectedLine [LineChunk "[1.00,1.00,1.00]"]]
 DocTest.printPrefix "Numeric.FFTW.Batch:153: "
{-# LINE 153 "src/Numeric/FFTW/Batch.hs" #-}
 DocTest.example
{-# LINE 153 "src/Numeric/FFTW/Batch.hs" #-}
   (floatList $ Batch.fourierCR $ Array.fromList ((), Shape.Cyclic (3::Int), Spectrum.Half (1::Int)) [1,0,0])
  [ExpectedLine [LineChunk "[1.00,1.00,1.00]"]]
