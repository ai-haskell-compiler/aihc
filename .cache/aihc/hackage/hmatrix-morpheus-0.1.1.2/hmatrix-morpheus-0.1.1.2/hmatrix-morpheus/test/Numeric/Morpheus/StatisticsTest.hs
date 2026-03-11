module Numeric.Morpheus.StatisticsTest
(
  tests
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Approx
import Test.HUnit.Plus

import Numeric.LinearAlgebra
import Numeric.Morpheus.Statistics


x = fromList [35,32,213,54,81,92,63,70]
meanValue = 80
stddevValue = 57.60456

a = matrix 4 [35,32,213,54,
              81,92,63,70,
              77,14,91,20]
aT = tr a

columnMeansRM = fromList [64.33333, 46, 122.33333, 48]
rowMeansRM = fromList [83.5, 76.5, 50.5]
columnMeansCM = rowMeansRM
rowMeansCM = columnMeansRM

columnStddevsRM = fromList [25.48202, 40.84116, 79.75797, 25.53429]
rowStddevsRM = fromList [86.88114, 12.71482, 39.17908]
columnStddevsCM = rowStddevsRM
rowStddevsCM = columnStddevsRM


tests = [ testGroup "Statistics" [
            testCase "mean" $ assertApproxEqual "" 1e-5 meanValue (mean x)
          , testCase "stddev_m" $ assertApproxEqual "" 1e-5 stddevValue (stddev_m meanValue x)
          , testCase "stddev" $ assertApproxEqual "" 1e-5 stddevValue (stddev x)
          ]
        , testGroup "Matrix Statistics" [
            testCase "columnMean - row major" $ assertVector "" 1e-5 columnMeansRM (columnMean a)
          , testCase "columnMean - column major" $ assertVector "" 1e-5 columnMeansCM (columnMean aT)
          , testCase "rowMean - row major" $ assertVector "" 1e-5 rowMeansRM (rowMean a)
          , testCase "rowMean - column major" $ assertVector "" 1e-5 rowMeansCM (rowMean aT)
          , testCase "columnStddev_m - row major" $ assertVector "" 1e-5 columnStddevsRM (columnStddev_m columnMeansRM a)
          , testCase "columnStddev_m - column major" $ assertVector "" 1e-5 columnStddevsCM (columnStddev_m columnMeansCM aT)
          , testCase "rowStddev_m - row major" $ assertVector "" 1e-5 rowStddevsRM (rowStddev_m rowMeansRM a)
          , testCase "rowStddev_m - column major" $ assertVector "" 1e-5 rowStddevsCM (rowStddev_m rowMeansCM aT)
          ]
        ]
