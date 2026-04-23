{- ORACLE_TEST pass -}
{-# LANGUAGE QualifiedDo #-}
module QualifiedDoLet where

f = M.do
  let x = 1
  return x
