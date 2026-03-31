{- ORACLE_TEST xfail nested multi-way if - known roundtrip issue -}
{-# LANGUAGE MultiWayIf #-}
module Nested where

f :: Int -> Int -> Int
f x y = if | x > 0 -> if | y > 0 -> x + y
                         | otherwise -> x
           | otherwise -> y