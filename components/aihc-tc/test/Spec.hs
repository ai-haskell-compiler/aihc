{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcGoldenTests, tcTests)

main :: IO ()
main = do
  golden <- tcGoldenTests
  defaultMain
    ( testGroup
        "aihc-tc"
        [ tcTests,
          golden,
          tcProperties
        ]
    )
