{- ORACLE_TEST
id: pattern
category: patterns
expected: xfail
reason: unboxed sum pattern
-}
{-# LANGUAGE UnboxedSums #-}
module Pattern where

f x = case x of
  (# | y #) -> y
  (# y | #) -> y
