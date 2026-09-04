module Main (main) where

import Test.Tasty
import Test.Tc.Interface (tcInterfaceTests)
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcAnnotatedGoldenTests)
import Test.Tc.Traverse (tcTraverseTests)

main :: IO ()
main = do
  annotatedGoldenTests <- tcAnnotatedGoldenTests
  traverseTests <- tcTraverseTests
  defaultMain
    ( testGroup
        "aihc-tc"
        [ annotatedGoldenTests,
          traverseTests,
          tcInterfaceTests,
          tcProperties
        ]
    )
