{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.Sampling

main :: IO ()
main = do
  foo <- resampleIO 100 ([1..100000] :: [Int])
  print foo
