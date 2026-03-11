{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Queue.Ephemeral (EphemeralQueue)
import Queue.Ephemeral qualified
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let n = 100000 :: Int
  defaultMain [bench "EphemeralQueue" (whnf theBenchmark n)]

theBenchmark :: Int -> Int
theBenchmark num =
  loop1 0 0 (Queue.Ephemeral.enqueue 0 Queue.Ephemeral.empty)
  where
    loop1 :: Int -> Int -> EphemeralQueue Int -> Int
    loop1 !n !acc queue
      | n == num = loop2 acc queue
      | otherwise =
          case Queue.Ephemeral.dequeue queue of
            Just (m, queue') -> loop1 (n + 1) (acc + m) (Queue.Ephemeral.enqueue n (Queue.Ephemeral.enqueue (n + 1) queue'))

    loop2 :: Int -> EphemeralQueue Int -> Int
    loop2 !n queue =
      case Queue.Ephemeral.dequeue queue of
        Nothing -> n
        Just (m, queue') -> loop2 (n + m) queue'
