-- Do not edit! Automatically created with doctest-extract from src/Numeric/FFTW/RankN.hs
{-# LINE 27 "src/Numeric/FFTW/RankN.hs" #-}

module Test.Numeric.FFTW.RankN where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 28 "src/Numeric/FFTW/RankN.hs" #-}
import     Test.Numeric.FFTW.Common
       (approxReal, approxComplex, floatTol, doubleTol,
        genCyclicArray1, genCyclicArray2, genCyclicArray3, immutable,
        arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble,
        floatList, complexFloatList)

import     qualified Numeric.FFTW.RankN as TrafoM
import     qualified Numeric.FFTW.Rank1 as Trafo1
import     qualified Numeric.FFTW.Rank2 as Trafo2
import     qualified Numeric.FFTW.Rank3 as Trafo3
import     qualified Numeric.FFTW.Shape as Spectrum

import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     Data.Array.Comfort.Storable (Array)

import     qualified Test.QuickCheck as QC

array1     :: (Shape.C sh) => Array sh a -> Array ((), sh) a
array1     = Array.mapShape ((,) ())

array3     :: (Shape.C sh0, Shape.C sh1, Shape.C sh2) =>
       Array (sh0,sh1,sh2) a -> Array ((sh0,sh1),sh2) a
array3     = Array.mapShape (\(sh0,sh1,sh2) -> ((sh0,sh1),sh2))

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.FFTW.RankN:59: "
{-# LINE 59 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 59 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs sign -> approxComplex floatTol (array1 $ Trafo1.fourier sign xs) (TrafoM.fourier sign $ array1 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:60: "
{-# LINE 60 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 60 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs sign -> approxComplex doubleTol (array1 $ Trafo1.fourier sign xs) (TrafoM.fourier sign $ array1 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:62: "
{-# LINE 62 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 62 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs sign -> approxComplex floatTol (Trafo2.fourier sign xs) (TrafoM.fourier sign xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:63: "
{-# LINE 63 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 63 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs sign -> approxComplex doubleTol (Trafo2.fourier sign xs) (TrafoM.fourier sign xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:65: "
{-# LINE 65 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 65 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs sign -> approxComplex floatTol (array3 $ Trafo3.fourier sign xs) (TrafoM.fourier sign $ array3 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:66: "
{-# LINE 66 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 66 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs sign -> approxComplex doubleTol (array3 $ Trafo3.fourier sign xs) (TrafoM.fourier sign $ array3 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:56: "
{-# LINE 56 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.example
{-# LINE 56 "src/Numeric/FFTW/RankN.hs" #-}
   (complexFloatList $ TrafoM.fourier TrafoM.Forward $ Array.fromList (Shape.Cyclic (5::Int), Shape.Cyclic (5::Int)) [0,0,0,0,0, 0,1,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0])
  [ExpectedLine [LineChunk "[1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95,1.00:+0.00,(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95,1.00:+0.00,0.31:+(-0.95),(-0.81):+0.59,0.31:+0.95,1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),0.31:+0.95,1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59]"]]
 DocTest.printPrefix "Numeric.FFTW.RankN:80: "
{-# LINE 80 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 80 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs -> approxComplex floatTol (array1 $ Trafo1.fourierRC xs) (TrafoM.fourierRC $ array1 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:81: "
{-# LINE 81 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 81 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs -> approxComplex doubleTol (array1 $ Trafo1.fourierRC xs) (TrafoM.fourierRC $ array1 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:83: "
{-# LINE 83 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 83 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs -> approxComplex floatTol (Trafo2.fourierRC xs) (TrafoM.fourierRC xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:84: "
{-# LINE 84 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 84 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs -> approxComplex doubleTol (Trafo2.fourierRC xs) (TrafoM.fourierRC xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:86: "
{-# LINE 86 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 86 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs -> approxComplex floatTol (array3 $ Trafo3.fourierRC xs) (TrafoM.fourierRC $ array3 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:87: "
{-# LINE 87 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 87 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs -> approxComplex doubleTol (array3 $ Trafo3.fourierRC xs) (TrafoM.fourierRC $ array3 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:90: "
{-# LINE 90 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 90 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray1 $ immutable TrafoM.fourierRC . array1 . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.RankN:91: "
{-# LINE 91 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 91 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray1 $ immutable TrafoM.fourierRC . array1 . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.RankN:93: "
{-# LINE 93 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 93 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray2 $ immutable TrafoM.fourierRC . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.RankN:94: "
{-# LINE 94 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 94 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray2 $ immutable TrafoM.fourierRC . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.RankN:96: "
{-# LINE 96 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 96 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray3 $ immutable TrafoM.fourierRC . array3 . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.RankN:97: "
{-# LINE 97 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 97 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll genCyclicArray3 $ immutable TrafoM.fourierRC . array3 . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.RankN:125: "
{-# LINE 125 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 125 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ \xs -> approxReal floatTol (array1 $ Trafo1.fourierCR xs) (TrafoM.fourierCR $ array1 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:126: "
{-# LINE 126 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 126 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ \xs -> approxReal doubleTol (array1 $ Trafo1.fourierCR xs) (TrafoM.fourierCR $ array1 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:128: "
{-# LINE 128 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 128 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ \xs -> approxReal floatTol (Trafo2.fourierCR xs) (TrafoM.fourierCR xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:129: "
{-# LINE 129 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 129 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ \xs -> approxReal doubleTol (Trafo2.fourierCR xs) (TrafoM.fourierCR xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:131: "
{-# LINE 131 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 131 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ \xs -> approxReal floatTol (array3 $ Trafo3.fourierCR xs) (TrafoM.fourierCR $ array3 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:132: "
{-# LINE 132 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 132 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ \xs -> approxReal doubleTol (array3 $ Trafo3.fourierCR xs) (TrafoM.fourierCR $ array3 xs))
 DocTest.printPrefix "Numeric.FFTW.RankN:135: "
{-# LINE 135 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 135 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ immutable TrafoM.fourierCR . array1 . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.RankN:136: "
{-# LINE 136 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 136 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ immutable TrafoM.fourierCR . array1 . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.RankN:138: "
{-# LINE 138 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 138 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable TrafoM.fourierCR . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.RankN:139: "
{-# LINE 139 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 139 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable TrafoM.fourierCR . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.RankN:141: "
{-# LINE 141 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 141 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable TrafoM.fourierCR . array3 . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.RankN:142: "
{-# LINE 142 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.property
{-# LINE 142 "src/Numeric/FFTW/RankN.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable TrafoM.fourierCR . array3 . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.RankN:115: "
{-# LINE 115 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.example
{-# LINE 115 "src/Numeric/FFTW/RankN.hs" #-}
   (floatList $ Trafo1.fourierCR $ Array.fromList (Spectrum.Half (3::Int)) [1,0 ])
  [ExpectedLine [LineChunk "[1.00,1.00,1.00]"]]
 DocTest.printPrefix "Numeric.FFTW.RankN:117: "
{-# LINE 117 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.example
{-# LINE 117 "src/Numeric/FFTW/RankN.hs" #-}
   (floatList $ TrafoM.fourierCR $ Array.fromList ((), Spectrum.Half (3::Int)) [1,0])
  [ExpectedLine [LineChunk "[1.00,1.00,1.00]"]]
 DocTest.printPrefix "Numeric.FFTW.RankN:120: "
{-# LINE 120 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.example
{-# LINE 120 "src/Numeric/FFTW/RankN.hs" #-}
   (floatList $ Trafo2.fourierCR $ Array.fromList (Shape.Cyclic (3::Int), Spectrum.Half (1::Int)) [1,0,0])
  [ExpectedLine [LineChunk "[1.00,1.00,1.00]"]]
 DocTest.printPrefix "Numeric.FFTW.RankN:122: "
{-# LINE 122 "src/Numeric/FFTW/RankN.hs" #-}
 DocTest.example
{-# LINE 122 "src/Numeric/FFTW/RankN.hs" #-}
   (floatList $ TrafoM.fourierCR $ Array.fromList (Shape.Cyclic (3::Int), Spectrum.Half (1::Int)) [1,0,0])
  [ExpectedLine [LineChunk "[1.00,1.00,1.00]"]]
