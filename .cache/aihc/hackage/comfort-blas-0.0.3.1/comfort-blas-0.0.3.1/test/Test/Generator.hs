{-# LANGUAGE GADTs #-}
module Test.Generator where

import qualified Numeric.Netlib.Class as Class
import qualified Numeric.BLAS.Scalar as Scalar
import Data.Complex (Complex((:+)))

import Control.Applicative (liftA2)

import qualified Test.QuickCheck as QC


genReal :: (Class.Real a) => Integer -> QC.Gen a
genReal maxElem = fmap fromIntegral $ QC.choose (-maxElem,maxElem)

genComplex :: (Class.Real a) => Integer -> QC.Gen (Complex a)
genComplex maxElem = liftA2 (:+) (genReal maxElem) (genReal maxElem)

genNumber :: (Class.Floating a) => Integer -> QC.Gen a
genNumber = genNumberAux Scalar.complexSingleton

genNumberAux ::
   (Class.Floating a) =>
   Scalar.ComplexSingleton a -> Integer -> QC.Gen a
genNumberAux singleton =
   case singleton of
      Scalar.Real -> genReal
      Scalar.Complex -> genComplex
