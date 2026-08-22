module Main (main) where

import Test.Tasty
import Test.Tc.Interface (tcInterfaceTests)
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcAnnotatedGoldenRegressionTests)

main :: IO ()
main = do
  annotatedGolden <- tcAnnotatedGoldenRegressionTests
  defaultMain
    ( testGroup
        "aihc-tc"
        [ annotatedGolden,
          tcInterfaceTests,
          tcProperties
        ]
    )
