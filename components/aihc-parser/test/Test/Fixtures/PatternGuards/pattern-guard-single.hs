{- ORACLE_TEST
id: pattern-guard-single
category: patterns
expected: pass
-}
{-# LANGUAGE PatternGuards #-}

module PatternGuardSingle where

headOrZero :: [Int] -> Int
headOrZero xs
  | y : _ <- xs = y
  | otherwise = 0
