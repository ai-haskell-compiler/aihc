{- ORACLE_TEST xfail aihc-parser does not support QualifiedDo bind syntax -}
{-# LANGUAGE QualifiedDo #-}
module QualifiedDoBind where

f = M.do
  x <- action
  return x
