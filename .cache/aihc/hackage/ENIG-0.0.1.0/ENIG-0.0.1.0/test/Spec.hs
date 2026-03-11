import qualified Test.Framework as Test

import Test.ENIG
import Test.ENIG.Detect
import Test.ENIG.Premise


main :: IO ()
main = do
  Test.defaultMain
    [ Test.ENIG.Premise.tests
    , Test.ENIG.Detect.tests
    , Test.ENIG.tests
    ]
