-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- License     : BSD-3-Clause
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
module Main (main) where

import Math.NumberTheory.Prime.Count
  ( nthPrime,
    primePhi,
    primePi,
    setNumPrimecountThreads,
  )
import Test.Tasty.Bench

primePiBenchmark :: Benchmark
primePiBenchmark =
  bgroup "primePi" $ do
    i <- [(1 :: Int) .. 12]
    pure $ bench ("10^" ++ show i) $ nf primePi (10 ^ i :: Int)

nthPrimeBenchmark :: Benchmark
nthPrimeBenchmark =
  bgroup "nthPrime" $ do
    i <- [(1 :: Int) .. 12]
    pure $ bench ("10^" ++ show i) $ nf nthPrime (10 ^ i :: Int)

primePhiBenchmark :: Benchmark
primePhiBenchmark =
  bgroup "primePhi" $ do
    i <- [(2 :: Int), 4 .. 12]
    j <- [(2 :: Int), 4 .. i]
    pure $
      bench ("n = 10^" ++ show i ++ ", a = 10^" ++ show j) $
        nf (primePhi (10 ^ i :: Int)) (10 ^ j :: Int)

main :: IO ()
main = do
  setNumPrimecountThreads 1
  defaultMain
    [ primePiBenchmark,
      nthPrimeBenchmark,
      primePhiBenchmark
    ]
