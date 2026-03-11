{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let n = 100000 :: Int
  defaultMain [bench "Seq" (whnf theBenchmark n)]

theBenchmark :: Int -> Int
theBenchmark num =
  loop1 0 0 (Seq.empty Seq.|> 0)
  where
    loop1 :: Int -> Int -> Seq Int -> Int
    loop1 !n !acc queue
      | n == num = loop2 acc queue
      | otherwise =
          case queue of
            m Seq.:<| queue' -> loop1 (n + 1) (acc + m) ((queue' Seq.|> (n + 1)) Seq.|> n)

    loop2 :: Int -> Seq Int -> Int
    loop2 !n = \case
      Seq.Empty -> n
      m Seq.:<| queue' -> loop2 (n + m) queue'
