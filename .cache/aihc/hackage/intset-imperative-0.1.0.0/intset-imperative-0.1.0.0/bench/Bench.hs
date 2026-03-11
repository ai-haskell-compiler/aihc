{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Exception

import Control.Monad

import Criterion.Main

import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.IntSet.Bounded.Imperative as BIS

import System.Random

main :: IO ()
main = defaultMain [
    bgroup "intset" [ let n = 1000;         !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (benchIntset 0 n v)
                    , let n = 10*1000;      !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (benchIntset 0 n v)
                    , let n = 100*1000;     !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (benchIntset 0 n v)
                    , let n = 1000*1000;    !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (benchIntset 0 n v)
                    , let n = 10*1000*1000; !v = generateInts 0 n (fromIntegral n)  in bench (show n)  $ whnfIO (benchIntset 0 n v)
                    ]
  ]

benchIntset :: Word64
             -> Word64
             -> Vector Word64
             -> IO ()
benchIntset minB maxB xs = do
    s <- BIS.empty minB maxB
    forM_ xs $ \i -> do
        BIS.insert s i
        f <- BIS.member s i
        unless f $
            throwIO $ userError "implementation errors"

generateInts :: Word64
             -> Word64
             -> Int
             -> Vector Word64
generateInts minB maxB n =
    V.fromList . take n $ randomRs (minB, maxB) (mkStdGen 0x214f36c9)
