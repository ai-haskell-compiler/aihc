{- ORACLE_TEST
id: type
category: types
expected: pass
reason: unboxed tuple type
-}
{-# LANGUAGE UnboxedTuples #-}
module Type where

f :: (# Int, Int #) -> (# Int, Int #)
f x = x
