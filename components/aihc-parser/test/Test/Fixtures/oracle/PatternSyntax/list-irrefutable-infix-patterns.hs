{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module ListInfixPatterns where

-- List patterns in infix function heads
[] `append` ys = ys
(x : xs) `append` ys = x : xs ++ ys

-- Irrefutable patterns in infix function heads  
~x `combine` y = x
x `combine` ~y = y
