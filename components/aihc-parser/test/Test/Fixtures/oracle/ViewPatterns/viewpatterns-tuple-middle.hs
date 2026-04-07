{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsTupleMiddle where

f :: (a, b -> c, d) -> b -> (a, c, d)
f (x, g -> y, z) b = (x, g b, z)
