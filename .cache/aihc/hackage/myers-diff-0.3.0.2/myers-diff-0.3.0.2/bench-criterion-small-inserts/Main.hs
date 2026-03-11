
module Main (main) where

import Criterion
import Criterion.Main
import Data.String.Interpolate
import TestLib.Benchmarking


numSamples :: Int
numSamples = 500

main :: IO ()
main = defaultMain [
  bgroup [i|Small insert|] [
             testGroup getPairSingleInsert numSamples 10
             , testGroup getPairSingleInsert numSamples 100
             , testGroup getPairSingleInsert numSamples 1000
             , testGroup getPairSingleInsert numSamples 10000
             , testGroup getPairSingleInsert numSamples 100000
             ]
  ]
