{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
-------------------------------------------------------------------------------
import           Data.RNG
-------------------------------------------------------------------------------


main :: IO ()
main = do
  !rng <- seedRNG 42
  defaultMain
    [ randomTokenBench rng
    , genIntBench rng
    ]


-------------------------------------------------------------------------------
randomTokenBench :: RNG -> Benchmark
randomTokenBench rng =
  bgroup "randomToken" (mkTest <$> [1, 10, 100])
  where
    mkTest n = bench (show n) (whnfIO (randomToken n rng))


-------------------------------------------------------------------------------
genIntBench :: RNG -> Benchmark
genIntBench rng =
  bench "generate Int" (whnfIO genInt)
  where
    genInt :: IO Int
    genInt = withRNG rng random
