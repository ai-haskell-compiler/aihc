-- | Benchmark the VLQ implementation.

module Main where

import Criterion.Main
import GHC.Int
import System.Random
import VLQ

-- | Runs a few randomly generated numbers (from the same seed each
-- time).
main :: IO ()
main = do
  defaultMain [benchmark]
  where benchmark = bgroup "numbers"
                           (map ((\n -> bench (show n) (whnf encode n)) .
                                 (fromIntegral :: Int -> Int32))
                             (take 5
                               (randomRs (1000000000,2000000000)
                                 (mkStdGen 1))))
