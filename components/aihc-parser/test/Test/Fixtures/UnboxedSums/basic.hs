{- ORACLE_TEST
id: basic
category: expressions
expected: xfail
reason: basic unboxed sum
-}
{-# LANGUAGE UnboxedSums #-}
module Basic where

x = (# 1 | #)
y = (# | 2 #)
