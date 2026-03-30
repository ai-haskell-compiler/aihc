{- ORACLE_TEST
id: basic
category: expressions
expected: xfail
reason: basic multi-way if
-}
{-# LANGUAGE MultiWayIf #-}
module Basic where

val :: Int -> String
val n = if | n < 0     -> "negative"
           | n == 0    -> "zero"
           | otherwise -> "positive"
