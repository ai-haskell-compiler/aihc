-- |
-- Module      :  Main
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sun Jun  7 10:49:26 2020.
module Main
  ( main,
  )
where

import Control.Monad
import Criterion.Main
import qualified Data.Vector.Unboxed as V
import Statistics.Gcm
import System.Random.MWC.Distributions
import System.Random.Stateful

genPredictors :: Int -> V.Vector Int
genPredictors n = V.iterateN n succ 0

genResponses :: StatefulGen g m => Int -> g -> m (V.Vector Double)
genResponses n g = V.fromList <$> replicateM n (exponential 1.0 g)

main :: IO ()
main = do
  g <- newIOGenM $ mkStdGen 0
  let p1e3 = genPredictors 1000
      p1e4 = genPredictors 10000
      p1e5 = genPredictors 100000
  d1e3 <- genResponses 1000 g
  d1e4 <- genResponses 10000 g
  d1e5 <- genResponses 100000 g
  defaultMain
    [ bgroup
        "Greatest convex minorant"
        [ bench "Vector of length 1e3" $ nf (gcm p1e3) d1e3,
          bench "Vector of length 1e4" $ nf (gcm p1e4) d1e4,
          bench "Vector of length 1e5" $ nf (gcm p1e5) d1e5
        ]
    ]

-- benchmarking Greatest convex minorant/Vector of length 1e3
-- time                 385.8 μs   (381.7 μs .. 390.9 μs)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 383.1 μs   (381.1 μs .. 388.0 μs)
-- std dev              10.02 μs   (4.799 μs .. 18.93 μs)
-- variance introduced by outliers: 19% (moderately inflated)

-- benchmarking Greatest convex minorant/Vector of length 1e4
-- time                 3.891 ms   (3.816 ms .. 3.985 ms)
--                      0.997 R²   (0.995 R² .. 0.999 R²)
-- mean                 3.942 ms   (3.893 ms .. 4.050 ms)
-- std dev              241.0 μs   (100.9 μs .. 467.2 μs)
-- variance introduced by outliers: 39% (moderately inflated)

-- benchmarking Greatest convex minorant/Vector of length 1e5
-- time                 37.99 ms   (37.88 ms .. 38.10 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 38.12 ms   (38.03 ms .. 38.42 ms)
-- std dev              327.3 μs   (109.8 μs .. 583.5 μs)
