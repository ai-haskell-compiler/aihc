{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcAnnotatedGoldenTests, tcTests)

main :: IO ()
main = do
  annotatedGolden <- tcAnnotatedGoldenTests
  defaultMain
    ( testGroup
        "aihc-tc"
        [ tcTests,
          annotatedGolden,
          tcProperties
        ]
    )
