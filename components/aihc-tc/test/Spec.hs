{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tc.Interface (tcInterfaceTests)
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcAnnotatedGoldenTests)

main :: IO ()
main = do
  annotatedGolden <- tcAnnotatedGoldenTests
  defaultMain
    ( testGroup
        "aihc-tc"
        [ annotatedGolden,
          tcInterfaceTests,
          tcProperties
        ]
    )
