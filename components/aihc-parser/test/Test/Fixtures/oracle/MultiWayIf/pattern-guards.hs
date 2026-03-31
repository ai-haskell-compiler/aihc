{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
module PatternGuards where

f :: Maybe Int -> Int
f m = if | Just x <- m, x > 0 -> x
         | otherwise          -> 0