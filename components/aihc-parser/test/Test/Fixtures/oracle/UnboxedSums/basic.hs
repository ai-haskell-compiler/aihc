{- ORACLE_TEST xfail basic unboxed sum -}
{-# LANGUAGE UnboxedSums #-}
module Basic where

x = (# 1 | #)
y = (# | 2 #)