{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsTupleSecond where

f :: (a, b -> c) -> b -> (a, c)
f (x, g -> y) b = (x, g b)
