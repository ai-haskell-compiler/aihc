{- ORACLE_TEST xfail basic multi-way if -}
{-# LANGUAGE MultiWayIf #-}
module Basic where

val :: Int -> String
val n = if | n < 0     -> "negative"
           | n == 0    -> "zero"
           | otherwise -> "positive"