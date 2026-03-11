module Test.Numeric.FFTW.Shape where

import qualified Numeric.FFTW.Shape as Spectrum
import Numeric.FFTW.Shape
         (SymmetrySingleton(Even,Odd), ShiftSingleton(Exact,Halfway))
import Test.Numeric.FFTW.Common (prefix)

import qualified Data.Array.Comfort.Shape.Test as ShapeTest

import qualified Test.QuickCheck as QC

import Control.Applicative ((<$>))


genHalf :: Int -> QC.Gen (Spectrum.Half Int)
genHalf n = Spectrum.Half <$> QC.choose (0,n)

genSymmetric ::
   (Spectrum.Symmetry symmetry) =>
   (Spectrum.Shift shiftTime) =>
   (Spectrum.Shift shiftSpectrum) =>
   Spectrum.SymmetrySingleton symmetry ->
   Spectrum.ShiftSingleton shiftTime ->
   Spectrum.ShiftSingleton shiftSpectrum ->
   Int -> QC.Gen (Spectrum.Symmetric symmetry shiftTime shiftSpectrum Int)
genSymmetric symmetry shiftTime shiftSpectrum n =
   Spectrum.Symmetric symmetry shiftTime shiftSpectrum <$> QC.choose (0,n)

tests :: [(String, QC.Property)]
tests =
   prefix "Half"
      (ShapeTest.tests $ genHalf 100) ++
   prefix "Half Even Exact Exact"
      (ShapeTest.tests $ genSymmetric Even Exact Exact 100) ++
   prefix "Half Even Halfway Exact"
      (ShapeTest.tests $ genSymmetric Even Halfway Exact 100) ++
   prefix "Half Even Exact Halfway"
      (ShapeTest.tests $ genSymmetric Even Exact Halfway 100) ++
   prefix "Half Even Halfway Halfway"
      (ShapeTest.tests $ genSymmetric Even Halfway Halfway 100) ++
   prefix "Half Odd Exact Exact"
      (ShapeTest.tests $ genSymmetric Odd Exact Exact 100) ++
   prefix "Half Odd Halfway Exact"
      (ShapeTest.tests $ genSymmetric Odd Halfway Exact 100) ++
   prefix "Half Odd Exact Halfway"
      (ShapeTest.tests $ genSymmetric Odd Exact Halfway 100) ++
   prefix "Half Odd Halfway Halfway"
      (ShapeTest.tests $ genSymmetric Odd Halfway Halfway 100) ++
   []
