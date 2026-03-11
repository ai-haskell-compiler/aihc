import Test.Framework (defaultMain, testGroup)

import qualified Numeric.Morpheus.ActivationTest as Activation
import qualified Numeric.Morpheus.MatrixReduceTest as MatrixReduce
import qualified Numeric.Morpheus.StatisticsTest as Statistics

main = defaultMain tests

tests = [ testGroup "Morpheus.Activation" Activation.tests
        , testGroup "Morpheus.MatrixReduce" MatrixReduce.tests
        , testGroup "Morpheus.Statistics" Statistics.tests
        ]
