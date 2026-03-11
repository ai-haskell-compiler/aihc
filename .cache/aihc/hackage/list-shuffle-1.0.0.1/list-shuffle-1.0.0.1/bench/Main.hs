module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import List.Shuffle qualified as List
import System.Random qualified as Random
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let list = [1 .. 1_000_000] :: [Int]
  _ <- evaluate (force list)

  defaultMain
    [ bench "sample 10/1000000" (whnf (sample 10) (list, Random.mkStdGen 0)),
      bench "shuffle 1000000" (whnf shuffle (list, Random.mkStdGen 0))
    ]

sample :: Int -> ([Int], Random.StdGen) -> [Int]
sample n (list, gen) =
  List.sample_ n list gen

shuffle :: ([Int], Random.StdGen) -> [Int]
shuffle (list, gen) =
  List.shuffle_ list gen
