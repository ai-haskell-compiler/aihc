module Main where

import ByteString.StrictBuilder
import Criterion.Main
import Prelude hiding (concat)

main :: IO ()
main =
  defaultMain [leftAppends, rightAppends, concat, copy]

leftAppends :: Benchmark
leftAppends =
  bench "leftAppends" $ whnf action $! replicate 1000 $ bytes "abc"
  where
    action bytesList =
      builderBytes builder
      where
        builder =
          foldl' (<>) mempty bytesList

rightAppends :: Benchmark
rightAppends =
  bench "rightAppends" $ whnf action $! replicate 1000 $ bytes "abc"
  where
    action bytesList =
      builderBytes builder
      where
        builder =
          foldr (<>) mempty bytesList

concat :: Benchmark
concat =
  bench "concat" $ whnf action $! replicate 10000 $ bytes "abc"
  where
    action bytesList =
      builderBytes (mconcat bytesList)

copy :: Benchmark
copy =
  bench "copy" $ whnf action $! bytes dat
  where
    dat = builderBytes $ mconcat $ replicate 1000000 $ bytes "abc"
    action bs =
      builderBytes bs
