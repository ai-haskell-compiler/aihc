module Main (main) where

import Test.Tasty
import Test.Tc.Interface (tcInterfaceTests)
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcAnnotatedGoldenTests)

main :: IO ()
main = do
  annotatedGoldenTests <- tcAnnotatedGoldenTests
  defaultMain
    ( testGroup
        "aihc-tc"
        [ annotatedGoldenTests,
          tcInterfaceTests,
          tcProperties
        ]
    )
