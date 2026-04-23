{- ORACLE_TEST pass -}
{-# LANGUAGE QualifiedDo #-}
module QualifiedDoBind where

f = M.do
  x <- action
  return x
