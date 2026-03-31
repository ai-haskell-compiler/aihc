{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedSums #-}
module Type where

f :: (# Int | Int #) -> Int
f (# x | #) = x
f (# | y #) = y