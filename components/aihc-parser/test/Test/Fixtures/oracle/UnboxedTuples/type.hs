{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedTuples #-}
module Type where

f :: (# Int, Int #) -> (# Int, Int #)
f x = x