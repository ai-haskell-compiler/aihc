{- ORACLE_TEST
id: type
category: types
expected: xfail
reason: unboxed sum type
-}
{-# LANGUAGE UnboxedSums #-}
module Type where

f :: (# Int | Int #) -> Int
f (# x | #) = x
f (# | y #) = y
