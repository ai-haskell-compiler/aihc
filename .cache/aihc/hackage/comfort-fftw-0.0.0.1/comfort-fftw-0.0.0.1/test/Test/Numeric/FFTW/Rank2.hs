-- Do not edit! Automatically created with doctest-extract from src/Numeric/FFTW/Rank2.hs
{-# LINE 25 "src/Numeric/FFTW/Rank2.hs" #-}

module Test.Numeric.FFTW.Rank2 where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 26 "src/Numeric/FFTW/Rank2.hs" #-}
import     Test.Numeric.FFTW.Common
       (approxReal, approxComplex, floatTol, doubleTol, normInf,
        adjust, scalarProduct, split, genCyclicArray2, immutable,
        arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble)

import     qualified Numeric.FFTW.Rank2 as Trafo2
import     Numeric.FFTW.Rank1 (flipSign)

import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Complex as Complex

import     qualified Test.QuickCheck as QC

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.FFTW.Rank2:42: "
{-# LINE 42 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 42 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs sign-> approxComplex floatTol (adjust xs) (Trafo2.fourier sign (Trafo2.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank2:43: "
{-# LINE 43 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 43 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs sign -> approxComplex doubleTol (adjust xs) (Trafo2.fourier sign (Trafo2.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank2:45: "
{-# LINE 45 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 45 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs sign -> approxComplex floatTol (Trafo2.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo2.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank2:46: "
{-# LINE 46 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 46 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs sign -> approxComplex doubleTol (Trafo2.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo2.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank2:48: "
{-# LINE 48 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 48 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xys sign -> let (xs,ys) = split xys in approxComplex floatTol (Trafo2.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo2.fourier sign xs) (Trafo2.fourier sign ys)))
 DocTest.printPrefix "Numeric.FFTW.Rank2:49: "
{-# LINE 49 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 49 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xys sign -> let (xs,ys) = split xys in approxComplex doubleTol (Trafo2.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo2.fourier sign xs) (Trafo2.fourier sign ys)))
 DocTest.printPrefix "Numeric.FFTW.Rank2:51: "
{-# LINE 51 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 51 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo2.fourier sign xs) (Trafo2.fourier sign ys)) <= floatTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys)
 DocTest.printPrefix "Numeric.FFTW.Rank2:52: "
{-# LINE 52 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 52 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo2.fourier sign xs) (Trafo2.fourier sign ys)) <= doubleTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys)
 DocTest.printPrefix "Numeric.FFTW.Rank2:54: "
{-# LINE 54 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 54 "src/Numeric/FFTW/Rank2.hs" #-}
     (\sign -> QC.forAll genCyclicArray2 $ immutable (Trafo2.fourier sign) . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank2:55: "
{-# LINE 55 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 55 "src/Numeric/FFTW/Rank2.hs" #-}
     (\sign -> QC.forAll genCyclicArray2 $ immutable (Trafo2.fourier sign) . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank2:70: "
{-# LINE 70 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 70 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs -> approxReal floatTol (adjust xs) (Trafo2.fourierCR (Trafo2.fourierRC xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank2:71: "
{-# LINE 71 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 71 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ \xs -> approxReal doubleTol (adjust xs) (Trafo2.fourierCR (Trafo2.fourierRC xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank2:73: "
{-# LINE 73 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 73 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ immutable Trafo2.fourierRC . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank2:74: "
{-# LINE 74 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 74 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll genCyclicArray2 $ immutable Trafo2.fourierRC . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank2:90: "
{-# LINE 90 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 90 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable Trafo2.fourierCR . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank2:91: "
{-# LINE 91 "src/Numeric/FFTW/Rank2.hs" #-}
 DocTest.property
{-# LINE 91 "src/Numeric/FFTW/Rank2.hs" #-}
     (QC.forAll (fmap Trafo2.fourierRC genCyclicArray2) $ immutable Trafo2.fourierCR . arrayComplexDouble)
