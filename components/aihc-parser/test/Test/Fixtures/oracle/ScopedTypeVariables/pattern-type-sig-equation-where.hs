{- ORACLE_TEST pass -}
{-# LANGUAGE ScopedTypeVariables #-}
module PatternTypeSigEquationWhere where

f :: Int = x
  where x = 42
