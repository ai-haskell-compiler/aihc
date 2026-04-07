{- ORACLE_TEST pass -}
module InlinablePragmas where

biggish :: Int -> Int
biggish n = sum (map (+1) [0 .. n])
{-# INLINABLE biggish #-}

altSpelling :: Int -> Int
altSpelling n = product [1 .. max 1 n]
{-# INLINEABLE [1] altSpelling #-}

inlinableUntilPhase :: Int -> Int
inlinableUntilPhase n = n * n
{-# INLINABLE [~1] inlinableUntilPhase #-}
