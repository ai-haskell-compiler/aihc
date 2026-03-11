-- Do not edit! Automatically created with doctest-extract from src/Numeric/FFTW/Rank1.hs
{-# LINE 29 "src/Numeric/FFTW/Rank1.hs" #-}

{-# OPTIONS_GHC -XGADTs #-}
module Test.Numeric.FFTW.Rank1 where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 31 "src/Numeric/FFTW/Rank1.hs" #-}
import     Test.Numeric.FFTW.Common
       (approxReal, approxComplex, floatTol, doubleTol, normInf,
        adjust, scalarProduct, split, genCyclicArray1, immutable,
        arrayFloat, arrayDouble, arrayComplexFloat, arrayComplexDouble,
        floatList, complexFloatList)

import     qualified Numeric.FFTW.Rank1 as Trafo1
import     qualified Numeric.FFTW.Shape as Spectrum
import     Numeric.FFTW.Rank1 (flipSign)
import     Numeric.FFTW.Shape
       (SymmetrySingleton(Even,Odd), ShiftSingleton(Exact,Halfway))

import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     qualified Data.Complex as Complex
import     Data.Array.Comfort.Storable (Array)

import     qualified Numeric.Netlib.Class as Class

import     Foreign.Storable (Storable)

import     qualified Test.QuickCheck as QC

genSymmetric     ::
       (Spectrum.Symmetry symmetry) =>
       (Spectrum.Shift shiftTime) =>
       (Spectrum.Shift shiftSpectrum) =>
       (Spectrum.Symmetric symmetry shiftTime shiftSpectrum Int ~ sh) =>
       (QC.Arbitrary a, Storable a) =>
       Spectrum.SymmetrySingleton symmetry ->
       Spectrum.ShiftSingleton shiftTime ->
       Spectrum.ShiftSingleton shiftSpectrum ->
       QC.Gen (Array sh a)
genSymmetric     symmetry shiftTime shiftSpectrum = do
       xs <- fmap (take 1000) $ QC.arbitrary
       return $ Array.fromList
          (Spectrum.Symmetric symmetry shiftTime shiftSpectrum $ length xs) xs

adjustSymmetric     ::
       (Spectrum.Symmetry symmetry) =>
       (Spectrum.Shift shiftTime) =>
       (Spectrum.Shift shiftSpectrum) =>
       (Spectrum.Symmetric symmetry shiftTime shiftSpectrum Int ~ sh) =>
       (Class.Floating a) =>
       Array sh a -> Array sh a
adjustSymmetric     xs =
       let n = Spectrum.symmetricLogicalSize (Array.shape xs)
       in Array.map (fromIntegral n *) xs

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.FFTW.Rank1:90: "
{-# LINE 90 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 90 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs sign-> approxComplex floatTol (adjust xs) (Trafo1.fourier sign (Trafo1.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:91: "
{-# LINE 91 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 91 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs sign -> approxComplex doubleTol (adjust xs) (Trafo1.fourier sign (Trafo1.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:93: "
{-# LINE 93 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 93 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs sign -> approxComplex floatTol (Trafo1.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo1.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:94: "
{-# LINE 94 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 94 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs sign -> approxComplex doubleTol (Trafo1.fourier sign (Array.map Complex.conjugate xs)) (Array.map Complex.conjugate (Trafo1.fourier (flipSign sign) xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:96: "
{-# LINE 96 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 96 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xys sign -> let (xs,ys) = split xys in approxComplex floatTol (Trafo1.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo1.fourier sign xs) (Trafo1.fourier sign ys)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:97: "
{-# LINE 97 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 97 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xys sign -> let (xs,ys) = split xys in approxComplex doubleTol (Trafo1.fourier sign $ Array.zipWith (+) xs ys) (Array.zipWith (+) (Trafo1.fourier sign xs) (Trafo1.fourier sign ys)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:99: "
{-# LINE 99 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 99 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo1.fourier sign xs) (Trafo1.fourier sign ys)) <= floatTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys)
 DocTest.printPrefix "Numeric.FFTW.Rank1:100: "
{-# LINE 100 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 100 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xys sign -> let (xs,ys) = split xys in Complex.magnitude (scalarProduct (adjust xs) ys - scalarProduct (Trafo1.fourier sign xs) (Trafo1.fourier sign ys)) <= doubleTol * normInf Complex.magnitude (adjust xs) * normInf Complex.magnitude ys)
 DocTest.printPrefix "Numeric.FFTW.Rank1:102: "
{-# LINE 102 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 102 "src/Numeric/FFTW/Rank1.hs" #-}
     (\sign -> QC.forAll genCyclicArray1 $ immutable (Trafo1.fourier sign) . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:103: "
{-# LINE 103 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 103 "src/Numeric/FFTW/Rank1.hs" #-}
     (\sign -> QC.forAll genCyclicArray1 $ immutable (Trafo1.fourier sign) . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:83: "
{-# LINE 83 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.example
{-# LINE 83 "src/Numeric/FFTW/Rank1.hs" #-}
   (complexFloatList $ Trafo1.fourier Trafo1.Forward $ Array.fromList (Shape.Cyclic (5::Int)) [1,0,0,0,0])
  [ExpectedLine [LineChunk "[1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00,1.00:+0.00]"]]
 DocTest.printPrefix "Numeric.FFTW.Rank1:85: "
{-# LINE 85 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.example
{-# LINE 85 "src/Numeric/FFTW/Rank1.hs" #-}
   (complexFloatList $ Trafo1.fourier Trafo1.Forward $ Array.fromList (Shape.Cyclic (5::Int)) [0,1,0,0,0])
  [ExpectedLine [LineChunk "[1.00:+0.00,0.31:+(-0.95),(-0.81):+(-0.59),(-0.81):+0.59,0.31:+0.95]"]]
 DocTest.printPrefix "Numeric.FFTW.Rank1:87: "
{-# LINE 87 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.example
{-# LINE 87 "src/Numeric/FFTW/Rank1.hs" #-}
   (complexFloatList $ Trafo1.fourier Trafo1.Backward $ Array.fromList (Shape.Cyclic (5::Int)) [0,1,0,0,0])
  [ExpectedLine [LineChunk "[1.00:+0.00,0.31:+0.95,(-0.81):+0.59,(-0.81):+(-0.59),0.31:+(-0.95)]"]]
 DocTest.printPrefix "Numeric.FFTW.Rank1:118: "
{-# LINE 118 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 118 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs -> approxReal floatTol (adjust xs) (Trafo1.fourierCR (Trafo1.fourierRC xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:119: "
{-# LINE 119 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 119 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs -> approxReal doubleTol (adjust xs) (Trafo1.fourierCR (Trafo1.fourierRC xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:121: "
{-# LINE 121 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 121 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ immutable Trafo1.fourierRC . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:122: "
{-# LINE 122 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 122 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ immutable Trafo1.fourierRC . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:140: "
{-# LINE 140 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 140 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ immutable Trafo1.fourierCR . arrayComplexFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:141: "
{-# LINE 141 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 141 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (fmap Trafo1.fourierRC genCyclicArray1) $ immutable Trafo1.fourierCR . arrayComplexDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:137: "
{-# LINE 137 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.example
{-# LINE 137 "src/Numeric/FFTW/Rank1.hs" #-}
   (floatList $ Trafo1.fourierCR $ Array.fromList (Spectrum.Half (5::Int)) [0,1,0])
  [ExpectedLine [LineChunk "[2.00,0.62,-1.62,-1.62,0.62]"]]
 DocTest.printPrefix "Numeric.FFTW.Rank1:159: "
{-# LINE 159 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 159 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Exact Exact `QC.suchThat` ((>=2) . Shape.size . Array.shape)) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:160: "
{-# LINE 160 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 160 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Exact Exact `QC.suchThat` ((>=2) . Shape.size . Array.shape)) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:161: "
{-# LINE 161 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 161 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Halfway Exact) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:162: "
{-# LINE 162 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 162 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Halfway Exact) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:163: "
{-# LINE 163 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 163 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Exact Halfway) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:164: "
{-# LINE 164 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 164 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Exact Halfway) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:165: "
{-# LINE 165 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 165 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Halfway Halfway) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:166: "
{-# LINE 166 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 166 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Halfway Halfway) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.cosine (Trafo1.cosine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:168: "
{-# LINE 168 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 168 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Exact Exact `QC.suchThat` ((>=2) . Shape.size . Array.shape)) $ immutable Trafo1.cosine . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:169: "
{-# LINE 169 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 169 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Exact Exact `QC.suchThat` ((>=2) . Shape.size . Array.shape)) $ immutable Trafo1.cosine . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:170: "
{-# LINE 170 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 170 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Halfway Exact) $ immutable Trafo1.cosine . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:171: "
{-# LINE 171 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 171 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Halfway Exact) $ immutable Trafo1.cosine . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:172: "
{-# LINE 172 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 172 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Exact Halfway) $ immutable Trafo1.cosine . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:173: "
{-# LINE 173 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 173 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Exact Halfway) $ immutable Trafo1.cosine . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:174: "
{-# LINE 174 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 174 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Halfway Halfway) $ immutable Trafo1.cosine . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:175: "
{-# LINE 175 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 175 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Even Halfway Halfway) $ immutable Trafo1.cosine . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:200: "
{-# LINE 200 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 200 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Exact Exact) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:201: "
{-# LINE 201 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 201 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Exact Exact) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:202: "
{-# LINE 202 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 202 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Halfway Exact) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:203: "
{-# LINE 203 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 203 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Halfway Exact) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:204: "
{-# LINE 204 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 204 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Exact Halfway) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:205: "
{-# LINE 205 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 205 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Exact Halfway) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:206: "
{-# LINE 206 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 206 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Halfway Halfway) $ \xs -> approxReal floatTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:207: "
{-# LINE 207 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 207 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Halfway Halfway) $ \xs -> approxReal doubleTol (adjustSymmetric xs) (Trafo1.sine (Trafo1.sine xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:209: "
{-# LINE 209 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 209 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Exact Exact) $ immutable Trafo1.sine . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:210: "
{-# LINE 210 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 210 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Exact Exact) $ immutable Trafo1.sine . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:211: "
{-# LINE 211 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 211 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Halfway Exact) $ immutable Trafo1.sine . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:212: "
{-# LINE 212 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 212 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Halfway Exact) $ immutable Trafo1.sine . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:213: "
{-# LINE 213 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 213 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Exact Halfway) $ immutable Trafo1.sine . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:214: "
{-# LINE 214 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 214 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Exact Halfway) $ immutable Trafo1.sine . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:215: "
{-# LINE 215 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 215 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Halfway Halfway) $ immutable Trafo1.sine . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:216: "
{-# LINE 216 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 216 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll (genSymmetric Odd Halfway Halfway) $ immutable Trafo1.sine . arrayDouble)
 DocTest.printPrefix "Numeric.FFTW.Rank1:239: "
{-# LINE 239 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 239 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs -> approxReal floatTol (adjust xs) (Trafo1.hartley (Trafo1.hartley xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:240: "
{-# LINE 240 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 240 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ \xs -> approxReal doubleTol (adjust xs) (Trafo1.hartley (Trafo1.hartley xs)))
 DocTest.printPrefix "Numeric.FFTW.Rank1:242: "
{-# LINE 242 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 242 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ immutable Trafo1.hartley . arrayFloat)
 DocTest.printPrefix "Numeric.FFTW.Rank1:243: "
{-# LINE 243 "src/Numeric/FFTW/Rank1.hs" #-}
 DocTest.property
{-# LINE 243 "src/Numeric/FFTW/Rank1.hs" #-}
     (QC.forAll genCyclicArray1 $ immutable Trafo1.hartley . arrayDouble)
