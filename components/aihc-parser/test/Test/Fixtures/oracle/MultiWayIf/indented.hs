{- ORACLE_TEST xfail indented multi-way if -}
{-# LANGUAGE MultiWayIf #-}
module Indented where

f :: Int -> Int -> Int
f x y = if
  | x > 0, y > 0 -> x + y
  | x < 0        -> -x
  | otherwise    -> y