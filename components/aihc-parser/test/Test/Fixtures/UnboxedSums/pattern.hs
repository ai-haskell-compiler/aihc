{- ORACLE_TEST xfail unboxed sum pattern -}
{-# LANGUAGE UnboxedSums #-}
module Pattern where

f x = case x of
  (# | y #) -> y
  (# y | #) -> y