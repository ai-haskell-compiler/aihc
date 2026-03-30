{- ORACLE_TEST
id: basic
category: expressions
expected: pass
reason: basic unboxed tuple
-}
{-# LANGUAGE UnboxedTuples #-}
module Basic where

x = (# 1, 2 #)
