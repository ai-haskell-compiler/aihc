{- ORACLE_TEST pass -}
{-# LANGUAGE PatternGuards #-}

module PatternGuardLetInExpression where

guardLetInExpr :: Int -> Int
guardLetInExpr n
  | let x = 1 in x > 0 = n
  | otherwise = 0