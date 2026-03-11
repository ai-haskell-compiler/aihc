{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Bench
-- Description :  Various benchmarks
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Dec 16 13:33:27 2019.
module Main where

import Control.Parallel.Strategies
import Criterion.Main
import Data.Bifunctor
import Data.Foldable
import ELynx.Tools.InputOutput
import ELynx.Tree
import ELynx.Tree.Simulate.PointProcess
import Length
import Lens
import System.Random.Stateful
import Tree

treeFileMany :: FilePath
treeFileMany = "data/Many.trees"

getManyTrees :: IO (Forest Phylo Name)
getManyTrees = parseFileWith (someNewick Standard) treeFileMany

hugeTree :: IO (Tree Length Int)
hugeTree = newIOGenM (mkStdGen 0) >>= simulateReconstructedTree 50000 Random 1.0 0.9

sinN :: Int -> Length -> Length
sinN n x = iterate sin x !! n

func :: Length -> Length
func = sinN 200

hugeTreeCalcPar :: Int -> Tree Length Int -> Tree Length Int
hugeTreeCalcPar n t = first func t `using` parTree n

main :: IO ()
main = do
  !ts <- getManyTrees
  !ht <- first getLength <$> hugeTree
  let !pht = lengthToPhyloTree ht
      !ht' = flipLabels ht
      mr1 = hugeTreeCalcPar 0 ht
      mr2 = hugeTreeCalcPar 1 ht
  if mr1 == mr2
    then putStrLn "Map OK."
    else do
      print mr1
      print mr2
      error "Map wrong."
  let fr1 = (foldl' (+) 0 . branches) ht
      fr2 = parBranchFoldMap 3 id (+) ht
  if 1e-8 > abs (fr1 - fr2)
    then putStrLn "Fold OK."
    else do
      print fr1
      print fr2
      error "Fold wrong."
  defaultMain
    [ bgroup "bipartition" [bench "manyTrees" $ nf (map bipartitions) ts],
      bgroup
        "map strategies"
        [ bench "sequential" $ nf (hugeTreeCalcPar 0) ht,
          bench "parallel 3" $ nf (hugeTreeCalcPar 3) ht
        ],
      bgroup
        "fold strategies"
        [ bench "sequential" $ nf (parBranchFoldMap 0 func (+)) ht,
          bench "parallel 3" $ nf (parBranchFoldMap 3 func (+)) ht
        ],
      -- Unsafe operations are fast, safe operations are roughly 50 percent slower.
      bgroup
        "length"
        [ bench "length sum foldl' safe" $ nf lengthSumFoldl' [0 .. 10000000],
          bench "length sum foldl' unsafe" $ nf lengthSumFoldl'Unsafe [0 .. 10000000],
          bench "length sum foldl' num instance" $ nf lengthSumFoldl'NumInstance [0 .. 10000000],
          bench "double sum foldl'" $ nf doubleSumFoldl' [0 .. 10000000],
          bench "double sum" $ nf doubleSum [0 .. 10000000]
        ],
      -- Lenses are fast.
      bgroup
        "lens"
        [ bench "sum with getter" $ nf sumWithGetter [0 .. 1000000 :: Length],
          bench "sum with accessor function" $ nf sumWithAccessorFunction [0 .. 1000000 :: Length],
          bench "sum with setter and getter" $ nf sumWithSetter [0 .. 1000000 :: Length],
          bench "sum with modify and accessor functions" $ nf sumWithModifyFunction [0 .. 1000000 :: Length]
        ],
      bgroup
        "[mono|bi][functor|foldable|traversable]"
        [ bench "normal fmap tree" $ nf fmapNormalFunctor ht',
          bench "mono fmap tree" $ nf fmapFunctor ht,
          bench "bi fmap tree" $ nf fmapBifunctor ht,
          bench "mono fold tree" $ nf totalBranchLengthFoldable ht,
          bench "bi fold tree" $ nf totalBranchLengthBifoldable ht,
          bench "mono traverse tree" $ nf toLengthTreeTraversable pht,
          bench "bi traverse tree" $ nf toLengthTreeBitraversable pht
        ]
    ]

-- Results 2020 Nov 2, commit aee6818.

-- benchmarking bipartition/manyTrees
-- time                 9.631 ms   (9.583 ms .. 9.721 ms)
--                      0.998 R²   (0.995 R² .. 1.000 R²)
-- mean                 9.723 ms   (9.670 ms .. 9.879 ms)
-- std dev              220.5 μs   (87.68 μs .. 421.0 μs)

-- benchmarking map strategies/sequential
-- time                 851.4 ms   (666.8 ms .. 1.025 s)
--                      0.994 R²   (0.978 R² .. 1.000 R²)
-- mean                 735.3 ms   (676.0 ms .. 766.2 ms)
-- std dev              66.42 ms   (55.94 ms .. 69.47 ms)
-- variance introduced by outliers: 22% (moderately inflated)

-- benchmarking map strategies/parallel 3
-- time                 362.1 ms   (348.5 ms .. 377.0 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 354.6 ms   (348.1 ms .. 358.2 ms)
-- std dev              4.959 ms   (1.499 ms .. 6.727 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- benchmarking fold strategies/sequential
-- time                 762.0 ms   (682.7 ms .. 913.8 ms)
--                      0.995 R²   (0.992 R² .. 1.000 R²)
-- mean                 676.3 ms   (647.5 ms .. 720.1 ms)
-- std dev              42.34 ms   (11.72 ms .. 57.27 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- benchmarking fold strategies/parallel 3
-- time                 413.4 ms   (330.2 ms .. 536.7 ms)
--                      0.990 R²   (0.972 R² .. 1.000 R²)
-- mean                 368.3 ms   (345.6 ms .. 394.6 ms)
-- std dev              25.80 ms   (7.999 ms .. 35.08 ms)
-- variance introduced by outliers: 20% (moderately inflated)

-- benchmarking length/length sum foldl' safe
-- time                 123.4 ms   (117.8 ms .. 136.4 ms)
--                      0.993 R²   (0.986 R² .. 1.000 R²)
-- mean                 120.3 ms   (118.1 ms .. 124.8 ms)
-- std dev              4.572 ms   (2.556 ms .. 6.511 ms)
-- variance introduced by outliers: 11% (moderately inflated)

-- benchmarking length/length sum foldl' unsafe
-- time                 82.04 ms   (77.92 ms .. 86.95 ms)
--                      0.994 R²   (0.988 R² .. 0.999 R²)
-- mean                 88.56 ms   (85.87 ms .. 92.70 ms)
-- std dev              6.079 ms   (4.635 ms .. 7.548 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- benchmarking length/length sum foldl' num instance
-- time                 102.4 ms   (89.16 ms .. 124.8 ms)
--                      0.964 R²   (0.896 R² .. 0.994 R²)
-- mean                 98.94 ms   (91.15 ms .. 106.4 ms)
-- std dev              11.90 ms   (8.779 ms .. 15.29 ms)
-- variance introduced by outliers: 32% (moderately inflated)

-- benchmarking length/double sum foldl'
-- time                 83.87 ms   (71.82 ms .. 104.0 ms)
--                      0.954 R²   (0.878 R² .. 0.996 R²)
-- mean                 103.1 ms   (95.66 ms .. 112.2 ms)
-- std dev              14.21 ms   (10.77 ms .. 17.84 ms)
-- variance introduced by outliers: 43% (moderately inflated)

-- benchmarking length/double sum
-- time                 102.6 ms   (88.13 ms .. 116.0 ms)
--                      0.970 R²   (0.938 R² .. 0.997 R²)
-- mean                 94.08 ms   (89.94 ms .. 100.9 ms)
-- std dev              8.071 ms   (5.233 ms .. 10.58 ms)
-- variance introduced by outliers: 21% (moderately inflated)

-- benchmarking lens/sum with getter
-- time                 4.320 ms   (4.303 ms .. 4.352 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 4.328 ms   (4.320 ms .. 4.346 ms)
-- std dev              38.06 μs   (18.94 μs .. 58.05 μs)

-- benchmarking lens/sum with accessor function
-- time                 4.314 ms   (4.073 ms .. 4.509 ms)
--                      0.912 R²   (0.795 R² .. 0.986 R²)
-- mean                 4.795 ms   (4.546 ms .. 5.264 ms)
-- std dev              1.056 ms   (562.7 μs .. 1.691 ms)
-- variance introduced by outliers: 90% (severely inflated)

-- benchmarking lens/sum with setter and getter
-- time                 16.34 ms   (14.93 ms .. 18.00 ms)
--                      0.955 R²   (0.913 R² .. 0.983 R²)
-- mean                 17.06 ms   (16.30 ms .. 18.25 ms)
-- std dev              2.137 ms   (1.656 ms .. 2.794 ms)
-- variance introduced by outliers: 59% (severely inflated)

-- benchmarking lens/sum with modify and accessor functions
-- time                 14.87 ms   (14.11 ms .. 15.68 ms)
--                      0.977 R²   (0.946 R² .. 0.993 R²)
-- mean                 15.54 ms   (15.15 ms .. 16.23 ms)
-- std dev              1.319 ms   (882.4 μs .. 2.030 ms)
-- variance introduced by outliers: 40% (moderately inflated)

-- Mono vs bi.
--
-- The Bifoldable instance is much slower.
--
-- The Bifunctor instance is as fast as the normal Functor instance.
--
-- The Bitraversable instance is fine, although slightly slower.

-- benchmarking [mono|bi][functor|foldable|traversable]/normal fmap tree
-- time                 30.57 ms   (29.87 ms .. 31.16 ms)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 29.84 ms   (29.66 ms .. 30.23 ms)
-- std dev              528.5 μs   (358.8 μs .. 725.0 μs)

-- benchmarking [mono|bi][functor|foldable|traversable]/mono fmap tree
-- time                 28.98 ms   (28.54 ms .. 29.36 ms)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 28.73 ms   (28.44 ms .. 29.40 ms)
-- std dev              901.2 μs   (411.2 μs .. 1.583 ms)
-- variance introduced by outliers: 10% (moderately inflated)

-- benchmarking [mono|bi][functor|foldable|traversable]/bi fmap tree
-- time                 29.53 ms   (28.27 ms .. 31.23 ms)
--                      0.993 R²   (0.984 R² .. 1.000 R²)
-- mean                 28.93 ms   (28.57 ms .. 29.55 ms)
-- std dev              1.103 ms   (436.8 μs .. 1.577 ms)
-- variance introduced by outliers: 10% (moderately inflated)

-- benchmarking [mono|bi][functor|foldable|traversable]/mono fold tree
-- time                 81.88 ms   (80.24 ms .. 83.03 ms)
--                      0.999 R²   (0.996 R² .. 1.000 R²)
-- mean                 83.68 ms   (82.70 ms .. 85.33 ms)
-- std dev              2.207 ms   (581.3 μs .. 2.915 ms)

-- benchmarking [mono|bi][functor|foldable|traversable]/bi fold tree
-- time                 124.1 ms   (120.7 ms .. 129.4 ms)
--                      0.999 R²   (0.996 R² .. 1.000 R²)
-- mean                 123.9 ms   (121.8 ms .. 127.5 ms)
-- std dev              4.128 ms   (2.139 ms .. 6.374 ms)
-- variance introduced by outliers: 11% (moderately inflated)

-- benchmarking [mono|bi][functor|foldable|traversable]/mono traverse tree
-- time                 118.5 ms   (115.2 ms .. 122.6 ms)
--                      0.999 R²   (0.995 R² .. 1.000 R²)
-- mean                 122.2 ms   (120.4 ms .. 126.8 ms)
-- std dev              4.086 ms   (1.704 ms .. 5.961 ms)
-- variance introduced by outliers: 11% (moderately inflated)

-- benchmarking [mono|bi][functor|foldable|traversable]/bi traverse tree
-- time                 122.6 ms   (116.8 ms .. 127.2 ms)
--                      0.999 R²   (0.997 R² .. 1.000 R²)
-- mean                 121.3 ms   (120.2 ms .. 123.0 ms)
-- std dev              2.377 ms   (1.043 ms .. 3.289 ms)
-- variance introduced by outliers: 11% (moderately inflated)
