{- ORACLE_TEST pass -}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsGuardQualifier where

f :: Maybe Int -> Int
f x
  | (id -> Just y) <- x = y
  | otherwise = 0
