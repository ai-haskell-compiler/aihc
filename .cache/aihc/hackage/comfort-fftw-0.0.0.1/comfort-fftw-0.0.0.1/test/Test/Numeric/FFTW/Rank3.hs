-- Do not edit! Automatically created with doctest-extract from src/Numeric/FFTW/Rank3.hs
{-# LINE 25 "src/Numeric/FFTW/Rank3.hs" #-}

module Test.Numeric.FFTW.Rank3 where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 26 "src/Numeric/FFTW/Rank3.hs" #-}
import     Test.Numeric.FFTW.Common
       (approxReal, approxComplex, floatTol, doubleTol, normInf,
        adjust, scalarProduct, split, genCyclicArray3, immutable,
        arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble)

import     qualified Numeric.FFTW.Rank3 as Trafo3
import     Numeric.FFTW.Rank1 (flipSign)

import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Complex as Complex

import     qualified Test.QuickCheck as QC

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.FFTW.Rank3:42: "
{-# LINE 42 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 42 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs sign-> approxComplex floatTol (adjust xs) (Trafo3.fourier sign (Trafo3.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank3:43: "
{-# LINE 43 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 43 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs sign -> approxComplex doubleTol (adjust xs) (Trafo3.fourier sign (Trafo3.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank3:45: "
{-# LINE 45 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 45 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs sign -> approxComplex floatTol (Trafo3.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo3.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank3:46: "
{-# LINE 46 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 46 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs sign -> approxComplex doubleTol (Trafo3.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo3.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank3:48: "
{-# LINE 48 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 48 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xys sign -> let (xs,ys) = split xys in approxComplex floatTol (Trafo3.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo3.fourier sign xs) (Trafo3.fourier sign ys)))
 DocTest.printPrefix "Numeric.FFTW.Rank3:49: "
{-# LINE 49 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 49 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xys sign -> let (xs,ys) = split xys in approxComplex doubleTol (Trafo3.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo3.fourier sign xs) (Trafo3.fourier sign ys)))
 DocTest.printPrefix "Numeric.FFTW.Rank3:51: "
{-# LINE 51 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 51 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo3.fourier sign xs) (Trafo3.fourier sign ys)) <= floatTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys)
 DocTest.printPrefix "Numeric.FFTW.Rank3:52: "
{-# LINE 52 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 52 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo3.fourier sign xs) (Trafo3.fourier sign ys)) <= doubleTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys)
 DocTest.printPrefix "Numeric.FFTW.Rank3:54: "
{-# LINE 54 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 54 "src/Numeric/FFTW/Rank3.hs" #-}
     (\sign -> QC.forAll genCyclicArray3 $ immutable (Trafo3.fourier sign) . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank3:55: "
{-# LINE 55 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 55 "src/Numeric/FFTW/Rank3.hs" #-}
     (\sign -> QC.forAll genCyclicArray3 $ immutable (Trafo3.fourier sign) . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank3:70: "
{-# LINE 70 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 70 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs -> approxReal floatTol (adjust xs) (Trafo3.fourierCR (Trafo3.fourierRC xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank3:71: "
{-# LINE 71 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 71 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ \xs -> approxReal doubleTol (adjust xs) (Trafo3.fourierCR (Trafo3.fourierRC xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank3:73: "
{-# LINE 73 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 73 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ immutable Trafo3.fourierRC . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank3:74: "
{-# LINE 74 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 74 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll genCyclicArray3 $ immutable Trafo3.fourierRC . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank3:92: "
{-# LINE 92 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 92 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable Trafo3.fourierCR . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank3:93: "
{-# LINE 93 "src/Numeric/FFTW/Rank3.hs" #-}
 DocTest.property
{-# LINE 93 "src/Numeric/FFTW/Rank3.hs" #-}
     (QC.forAll (fmap Trafo3.fourierRC genCyclicArray3) $ immutable Trafo3.fourierCR . arrayComplexDouble)
