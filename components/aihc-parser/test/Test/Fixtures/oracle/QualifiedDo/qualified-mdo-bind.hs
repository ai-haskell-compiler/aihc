{- ORACLE_TEST pass -}
{-# LANGUAGE QualifiedDo, RecursiveDo #-}
module QualifiedMdoBind where

f = M.mdo
  x <- action
  return x
