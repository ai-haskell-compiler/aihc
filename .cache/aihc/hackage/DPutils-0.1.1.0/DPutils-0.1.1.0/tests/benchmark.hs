
module Main where

import Criterion.Main
import Data.Vector as V

import Math.TriangularNumbers



-- | Run 

benchToLinear :: Int -> Int -> Int
benchToLinear i j = V.sum $ V.map (\z -> toLinear z (i,j)) $ V.enumFromN 10 1000
{-# NoInline benchToLinear #-}

benchFromLinear :: Int -> Int
benchFromLinear k = V.sum $ V.map (\z -> let (i,j) = fromLinear z k in i+j) $ V.enumFromN 10 1000
{-# NoInline benchFromLinear #-}

--

main :: IO ()
main = defaultMain
  [ bench "toLinear"   $ nf (toLinear 10) (4,7)
  , bench "fromLinear" $ nf (fromLinear 10) 41
  , bench "benchToLinear"   $ nf (benchToLinear 4) 7
  , bench "benchFromLinear" $ nf benchFromLinear 41
  ]

