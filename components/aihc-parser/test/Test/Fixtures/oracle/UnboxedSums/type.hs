{- ORACLE_TEST xfail unboxed sum type -}
{-# LANGUAGE UnboxedSums #-}
module Type where

f :: (# Int | Int #) -> Int
f (# x | #) = x
f (# | y #) = y