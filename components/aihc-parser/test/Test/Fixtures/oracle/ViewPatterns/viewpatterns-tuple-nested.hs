{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsTupleNested where

f :: (a -> b, (c -> d, e)) -> a -> c -> (b, d, e)
f (g -> x, (h -> y, z)) a c = (g a, h c, z)
