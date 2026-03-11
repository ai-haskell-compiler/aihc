{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.Word
import HaskellWorks.Data.Excess.MinMaxExcess0
import HaskellWorks.Data.Excess.MinMaxExcess1
import HaskellWorks.Data.Excess.PartialMinMaxExcess0
import HaskellWorks.Data.Excess.PartialMinMaxExcess1
import HaskellWorks.Data.Excess.Triplet
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Vector.Storable     as DVS

setupEnvExcess :: Int -> IO (DVS.Vector Word64)
setupEnvExcess n = do
  lbs <- LBS.readFile "/dev/random"
  return (asVector64 (LBS.toStrict (LBS.take (fromIntegral n) lbs)))

runPartialMinMaxExcess0Vector :: DVS.Vector Word64 -> IO ()
runPartialMinMaxExcess0Vector v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where c = fromIntegral b :: Word64
                Triplet lo ex hi = partialMinMaxExcess0 64 c

runPartialMinMaxExcess1Vector :: DVS.Vector Word64 -> IO ()
runPartialMinMaxExcess1Vector v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where c = fromIntegral b :: Word64
                Triplet lo ex hi = partialMinMaxExcess1 64 c

runMinMaxExcess0VectorElems :: DVS.Vector Word64 -> IO ()
runMinMaxExcess0VectorElems v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where Triplet lo ex hi  = minMaxExcess0 (b :: Word64)

runMinMaxExcess1VectorElems :: DVS.Vector Word64 -> IO ()
runMinMaxExcess1VectorElems v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where Triplet lo ex hi  = minMaxExcess1 (b :: Word64)

runMinMaxExcess0Vector :: DVS.Vector Word64 -> IO ()
runMinMaxExcess0Vector v = do
  let !_ = minMaxExcess1 v

  return ()

runMinMaxExcess1Vector :: DVS.Vector Word64 -> IO ()
runMinMaxExcess1Vector v = do
  let !_ = minMaxExcess1 v

  return ()

makeBenchExcess :: IO [Benchmark]
makeBenchExcess = return $
  [ env (setupEnvExcess (1024 * 1024)) $ \v -> bgroup "MinMaxExcess Vector"
    [ bench "MinMaxExcess0" (whnfIO (runMinMaxExcess0VectorElems v))
    , bench "MinMaxExcess1" (whnfIO (runMinMaxExcess1VectorElems v))
    , bench "MinMaxExcess0" (whnfIO (runMinMaxExcess0Vector      v))
    , bench "MinMaxExcess1" (whnfIO (runMinMaxExcess1Vector      v))
    ]
  , env (setupEnvExcess (1024 * 1024)) $ \v -> bgroup "PartialMinMaxExcess Vector"
    [ bench "PartialMinMaxExcess0" (whnfIO (runPartialMinMaxExcess0Vector v))
    , bench "PartialMinMaxExcess1" (whnfIO (runPartialMinMaxExcess1Vector v))
    ]
  ]

main :: IO ()
main = do
  benchmarks <- mconcat <$> sequence
    [ makeBenchExcess
    ]
  defaultMain benchmarks
