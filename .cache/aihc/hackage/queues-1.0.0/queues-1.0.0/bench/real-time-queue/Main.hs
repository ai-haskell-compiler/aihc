{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Queue (Queue)
import Queue qualified
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let n = 100000 :: Int
  defaultMain [bench "Queue" (whnf theBenchmark n)]

theBenchmark :: Int -> Int
theBenchmark num =
  loop1 0 0 (Queue.enqueue 0 Queue.empty)
  where
    loop1 :: Int -> Int -> Queue Int -> Int
    loop1 !n !acc queue
      | n == num = loop2 acc queue
      | otherwise =
          case Queue.dequeue queue of
            Just (m, queue') -> loop1 (n + 1) (acc + m) (Queue.enqueue n (Queue.enqueue (n + 1) queue'))

    loop2 :: Int -> Queue Int -> Int
    loop2 !n queue =
      case Queue.dequeue queue of
        Nothing -> n
        Just (m, queue') -> loop2 (n + m) queue'
