{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Criterion.Main
import Numeric.Sampling (sampleIO, resampleIO)

main :: IO ()
main = defaultMain [
  env (return ([1..10000] :: [Int])) $ \x ->
      bgroup "benchmarks" [
        bench "sample"   $ nfIO (sampleIO 100 x)
      , bench "resample" $ nfIO (resampleIO 100 x)
      ]
    ]

