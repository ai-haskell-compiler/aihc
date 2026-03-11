module Numeric.Morpheus.MatrixReduceTest
(
  tests
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Plus

import Numeric.LinearAlgebra
import Numeric.Morpheus.MatrixReduce


xRM = matrix 5 [
  71, 11, 3,  9, 7,
  21, 7,  2,  23, 11,
  11, 32, 53, 49, 37,
  1,  24, 78, 90, 17
  ]

xCM = tr xRM

columnSumRM = vector [104, 74, 136, 171, 72]
columnSumCM = vector [101, 64,182, 210]

rowSumRM = vector [101, 64,182, 210]
rowSumCM = vector [104, 74, 136, 171, 72]

columnMaxValuesRM = fromList [71, 32, 78, 90, 37]
columnMaxIndicesRM = fromList [0, 2, 3, 3, 2]
columnMaxValuesCM = fromList [71, 23, 53, 90]
columnMaxIndicesCM = fromList [0, 3, 2, 3]

columnMinValuesRM = fromList [1, 7, 2, 9, 7]
columnMinIndicesRM = fromList [3, 1, 1, 0, 0]
columnMinValuesCM = fromList [3, 2, 11, 1]
columnMinIndicesCM = fromList [2, 2, 0, 0]

rowMaxValuesRM = fromList [71, 23, 53, 90]
rowMaxIndicesRM = fromList [0, 3, 2, 3]
rowMaxValuesCM = fromList [71, 32, 78, 90, 37]
rowMaxIndicesCM = fromList [0, 2, 3, 3, 2]

rowMinValuesRM = fromList [3, 2, 11, 1]
rowMinIndicesRM = fromList [2, 2, 0, 0]
rowMinValuesCM = fromList [1, 7, 2, 9, 7]
rowMinIndicesCM = fromList [3, 1, 1, 0, 0]


assertValuesAndIndices (expectedR, expectedI) (actualR, actualI) = do
  assertVector "values" 1e-10 expectedR actualR
  assertVectorI "indices" expectedI actualI


tests = [testGroup "Column Sum" [
            testCase "row major" $ assertVector "" 1e-10 columnSumRM (columnSum xRM)
          , testCase "column major" $ assertVector "" 1e-10 columnSumCM (columnSum xCM)
          ]
        , testGroup "Row Sum" [
            testCase "row major" $ assertVector "" 1e-10 rowSumRM (rowSum xRM)
          , testCase "column major" $ assertVector "" 1e-10 rowSumCM (rowSum xCM)
          ]
        , testGroup "Column Max Index" [
            testCase "row major" $ assertValuesAndIndices (columnMaxValuesRM, columnMaxIndicesRM) (columnMaxIndex xRM)
          , testCase "column major" $ assertValuesAndIndices (columnMaxValuesCM, columnMaxIndicesCM) (columnMaxIndex xCM)
          ]
        , testGroup "Column Min Index" [
            testCase "row major" $ assertValuesAndIndices (columnMinValuesRM, columnMinIndicesRM) (columnMinIndex xRM)
          , testCase "column major" $ assertValuesAndIndices (columnMinValuesCM, columnMinIndicesCM) (columnMinIndex xCM)
          ]
        , testGroup "Row Max Index" [
            testCase "row major" $ assertValuesAndIndices (rowMaxValuesRM, rowMaxIndicesRM) (rowMaxIndex xRM)
          , testCase "column major" $ assertValuesAndIndices (rowMaxValuesCM, rowMaxIndicesCM) (rowMaxIndex xCM)
          ]
        , testGroup "Row Min Index" [
            testCase "row major" $ assertValuesAndIndices (rowMinValuesRM, rowMinIndicesRM) (rowMinIndex xRM)
          , testCase "column major" $ assertValuesAndIndices (rowMinValuesCM, rowMinIndicesCM) (rowMinIndex xCM)
          ]
        , testGroup "Column Predicate" [
            testCase "max - row major" $ assertVector "" 1e-10 columnMaxValuesRM (columnPredicate max xRM)
          , testCase "max - col major" $ assertVector "" 1e-10 columnMaxValuesCM (columnPredicate max xCM)
          ]
          , testGroup "Row Predicate" [
              testCase "min - row major" $ assertVector "" 1e-10 rowMinValuesRM (rowPredicate min xRM)
            , testCase "min - col major" $ assertVector "" 1e-10 rowMinValuesCM (rowPredicate min xCM)
            ]
        ]
