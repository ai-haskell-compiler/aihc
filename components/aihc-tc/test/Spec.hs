{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcTests)

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "aihc-tc"
        [ tcTests,
          tcProperties
        ]
    )
