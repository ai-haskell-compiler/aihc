-- |
-- Module      :  Spec
-- Description :  Covariance test suite
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  9 21:59:03 2021.
module Main
  ( main,
  )
where

import Data.Either
import qualified Numeric.LinearAlgebra as L
import Statistics.Covariance
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests

emptyM :: L.Matrix Double
emptyM = L.fromLists [[]]

oneSampleM :: L.Matrix Double
oneSampleM = L.fromLists [[1, 2, 3, 4, 5]]

type Estimator = (L.Matrix Double -> Either String (L.Herm Double), String)

unitTestsForEstimator :: Estimator -> [TestTree]
unitTestsForEstimator (e, m) =
  [ testCase (m <> " fails on empty data matrices.") $
      assertEqual "" True (isLeft $ e emptyM),
    testCase (m <> " fails on one sample data matrices.") $
      assertEqual "" True (isLeft $ raoBlackwellLedoitWolf oneSampleM)
  ]

unitTestsForEstimators :: [Estimator] -> [TestTree]
unitTestsForEstimators = concatMap unitTestsForEstimator

estimators :: [Estimator]
estimators =
  [ (ledoitWolf DoCenter, "ledoitWolf"),
    (raoBlackwellLedoitWolf, "raoBlackwellLedoitWolf"),
    (oracleApproximatingShrinkage, "oracleApproximatingShrinkage"),
    (fmap fst . graphicalLasso 1.0, "graphicalLasso")
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" $ unitTestsForEstimators estimators

-- import qualified Numeric.LinearAlgebra as L
-- let m = L.gaussianSample 666 30 (L.fromList [0..40]) (L.trustSym $ L.diagl [1..41])
