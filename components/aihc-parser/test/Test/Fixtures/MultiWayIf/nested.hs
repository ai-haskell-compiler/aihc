{- ORACLE_TEST
id: nested
category: expressions
expected: xfail
reason: nested multi-way if
-}
{-# LANGUAGE MultiWayIf #-}
module Nested where

f :: Int -> Int -> Int
f x y = if | x > 0 -> if | y > 0 -> x + y
                         | otherwise -> x
           | otherwise -> y
