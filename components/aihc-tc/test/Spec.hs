{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcAnnotationGoldenTests, tcGoldenTests, tcTests)

main :: IO ()
main = do
  golden <- tcGoldenTests
  annotationGolden <- tcAnnotationGoldenTests
  defaultMain
    ( testGroup
        "aihc-tc"
        [ tcTests,
          golden,
          annotationGolden,
          tcProperties
        ]
    )
