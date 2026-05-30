{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcAnnotatedGoldenTests, tcGoldenTests, tcTests)

main :: IO ()
main = do
  golden <- tcGoldenTests
  annotatedGolden <- tcAnnotatedGoldenTests
  defaultMain
    ( testGroup
        "aihc-tc"
        [ tcTests,
          golden,
          annotatedGolden,
          tcProperties
        ]
    )
