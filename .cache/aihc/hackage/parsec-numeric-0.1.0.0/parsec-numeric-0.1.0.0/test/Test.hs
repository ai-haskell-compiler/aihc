import Test.Tasty

import qualified Test.Text.ParserCombinators.Parsec.Numeric

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
  [ Test.Text.ParserCombinators.Parsec.Numeric.tests
  ]
