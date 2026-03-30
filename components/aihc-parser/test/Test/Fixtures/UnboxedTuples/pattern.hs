{- ORACLE_TEST
id: pattern
category: patterns
expected: pass
reason: unboxed tuple pattern
-}
{-# LANGUAGE UnboxedTuples #-}
module Pattern where

f (# x, y #) = x
