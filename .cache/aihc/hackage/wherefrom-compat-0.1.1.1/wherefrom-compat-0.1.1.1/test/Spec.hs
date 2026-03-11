module Main (main) where
import Test.Tasty
import Test.Tasty.HUnit

import GHC.InfoProv.Compat

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "whereFrom works" $ do
     v <- whereFrom main
     assertBool "whereFrom shouldn't be Nothing" $ v /= Nothing
  ]
