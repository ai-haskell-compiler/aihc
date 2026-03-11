{-# LANGUAGE TypeApplications #-}

module Tests.Spline (
    splineTest
  ) where

import           Data.Maybe
import           Numeric.EMD.Internal.Spline
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Map                    as M
import qualified Data.Set                    as S

splineTest :: TestTree
splineTest = testCase "Spline Test" $ do
    expected <- map (read @Double) . lines <$> readFile "test-data/sintest.csv"
    roundOut expected @=? roundOut samples
  where
    roundOut :: [Double] -> [Double]
    roundOut = map $ (/ 10e12) . fromInteger . round . (* 10e12)
    spline :: Spline Double
    spline = fromJust
           . makeSpline SENotAKnot
           . M.fromSet sin
           . S.fromList
           $ [0, 1, 2.5, 3.6, 5, 7, 8.1, 10]
    samples :: [Double]
    samples = sampleSpline spline . (/ 4) . fromInteger <$> [0..40]
