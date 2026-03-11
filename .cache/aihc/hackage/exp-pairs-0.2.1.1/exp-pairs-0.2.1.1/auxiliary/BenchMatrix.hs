module Main where

import qualified Math.ExpPairs.Matrix3 as M3
import Test.Tasty.Bench

testm3 :: Int -> M3.Matrix3 Integer
testm3 k = M3.fromList $ map (100*10^k `div`) [100..108]

compareMults :: Int -> Benchmark
compareMults k = bgroup (show k)
  [ bench "vanillaMult"  $ nf (\x -> x * x)                 (testm3 k)
  , bench "makarovMult"  $ nf (\x -> x `M3.makarovMult` x)  (testm3 k)
  , bench "ladermanMult" $ nf (\x -> x `M3.ladermanMult` x) (testm3 k)
  ]

main :: IO ()
main = defaultMain $ map compareMults [400,450..800]
