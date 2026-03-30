{- ORACLE_TEST
id: pattern-guards
category: expressions
expected: xfail
reason: pattern guards in multi-way if
-}
{-# LANGUAGE MultiWayIf #-}
module PatternGuards where

f :: Maybe Int -> Int
f m = if | Just x <- m, x > 0 -> x
         | otherwise          -> 0
