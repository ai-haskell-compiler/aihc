import qualified LinearForm (testSuite)
import qualified Matrix3 (testSuite)
import qualified RatioInf (testSuite)
import qualified Pair (testSuite)
import qualified PrettyProcess (testSuite)
import qualified Process (testSuite)

import qualified Ivic (testSuite)
import qualified Kratzel (testSuite)
import qualified MenzerNowak (testSuite)

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Matrix3.testSuite
  , LinearForm.testSuite
  , RatioInf.testSuite
  , Pair.testSuite
  , PrettyProcess.testSuite
  , Process.testSuite
  , Ivic.testSuite
  , Kratzel.testSuite
  , MenzerNowak.testSuite
  ]
