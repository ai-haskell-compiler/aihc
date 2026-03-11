-- |
-- Module      :  Main
-- Description :  Benchmark circular stacks
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jun 20 21:12:38 2020.
module Main
  ( main,
  )
where

import Control.Monad.ST
import Criterion.Main
import Data.Foldable
import qualified Data.Stack.Circular as C
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- When using foldl or foldl', list is much slower than cstack.

listFoldL :: Int -> Int
listFoldL l = sum $ take 1000 $ foldl (flip (:)) [] [0 .. l]

cstackV :: Int -> Int
cstackV l = runST $ do
  c <- C.replicate 1000 0 :: ST s (C.MStack V.Vector s Int)
  c' <- foldlM (flip C.push) c [0 .. l]
  C.foldM (+) 0 c'

cstackU :: Int -> Int
cstackU l = runST $ do
  c <- C.replicate 1000 0 :: ST s (C.MStack U.Vector s Int)
  c' <- foldlM (flip C.push) c [0 .. l]
  C.foldM (+) 0 c'

-- When using foldr, cstack is slower by far. This is because list are lazy.

main :: IO ()
main = do
  let l = 1000000 :: Int
  print $ listFoldL l
  print $ cstackU l
  defaultMain
    [ bench "list, foldl" $ whnf listFoldL l,
      bench "cstack, foldl" $ whnf cstackV l,
      bench "cstack unboxed, foldl" $ whnf cstackU l
    ]

-- benchmarking list, foldl
-- time                 81.05 ms   (77.39 ms .. 85.31 ms)
--                      0.995 R²   (0.989 R² .. 0.999 R²)
-- mean                 86.91 ms   (83.37 ms .. 96.53 ms)
-- std dev              9.154 ms   (2.550 ms .. 15.87 ms)
-- variance introduced by outliers: 29% (moderately inflated)

-- benchmarking cstack, foldl
-- time                 14.92 ms   (14.82 ms .. 14.98 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 15.08 ms   (14.99 ms .. 15.55 ms)
-- std dev              464.1 μs   (31.62 μs .. 1.007 ms)
-- variance introduced by outliers: 11% (moderately inflated)

-- benchmarking cstack unboxed, foldl
-- time                 13.70 ms   (13.64 ms .. 13.77 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 13.66 ms   (13.64 ms .. 13.69 ms)
-- std dev              62.64 μs   (44.97 μs .. 86.20 μs)
