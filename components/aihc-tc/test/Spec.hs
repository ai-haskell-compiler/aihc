module Main (main) where

import Test.Tasty
import Test.Tc.Interface (tcInterfaceTests)
import Test.Tc.Properties (tcProperties)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "aihc-tc"
        [ tcInterfaceTests,
          tcProperties
        ]
    )
