module Main where

import Criterion.Main
import Data.Duration

main :: IO ()
main = defaultMain
  [ bgroup "to human"
    [ bench "small" $ nf humanReadableDuration 0.123456
    , bench "Basic" $ nf humanReadableDuration 987654321.123456]
  , bgroup "from human back to human"
    [ bench "long" $ nf humanReadableDuration
       (10*ms + 21*oneSecond + 42*minute + 3*hour + 222*day + 45*year)
    ]
  ]
