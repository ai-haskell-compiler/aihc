module Test.NumericPrelude.Utility where

import qualified Test.QuickCheck as QC

import qualified NumericPrelude.Numeric as NP

import Data.Eq.HT (equating)


equalTrunc :: Int -> [NP.Rational] -> [NP.Rational] -> Bool
equalTrunc n = equating (take n)


infixr 0 /\

(/\) :: (Show a, QC.Testable test) => QC.Gen a -> (a -> test) -> QC.Property
(/\) = QC.forAll
