module Main (main) where

import Test.Tasty
import Test.Tc.Interface (tcInterfaceTests)
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcTypeKindGoldenTests)

main :: IO ()
main = do
  kindTests <- tcTypeKindGoldenTests
  defaultMain
    ( testGroup
        "aihc-tc"
        [ kindTests,
          tcInterfaceTests,
          tcProperties
        ]
    )
