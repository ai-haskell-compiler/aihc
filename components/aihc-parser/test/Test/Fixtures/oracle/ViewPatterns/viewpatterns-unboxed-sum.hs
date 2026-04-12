{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedSums #-}
{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsUnboxedSum where

-- View pattern inside an unboxed sum pattern.
-- The -> is the view pattern arrow; the | is the unboxed sum separator.
f :: (# Int | () #) -> Int
f (# g -> x | #) = x
