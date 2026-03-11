
module Main (main) where

import Criterion
import Criterion.Main
import Data.String.Interpolate
import TestLib.Benchmarking


numSamples :: Int
numSamples = 500

main :: IO ()
main = defaultMain [
  bgroup [i|Small delete|] [
             testGroup getPairSingleDelete numSamples 10
             , testGroup getPairSingleDelete numSamples 100
             , testGroup getPairSingleDelete numSamples 1000
             , testGroup getPairSingleDelete numSamples 10000
             , testGroup getPairSingleDelete numSamples 100000
             ]
  ]
