{- ORACLE_TEST pass -}
{-# LANGUAGE ParallelListComp #-}
{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedSums #-}

module UnboxedSumListComp where

-- List comprehension nested inside an unboxed sum expression.
-- The first | is the comprehension qualifier separator;
-- the second | is the unboxed sum alternative separator.
x src = (# [e | e <- src] | #)
