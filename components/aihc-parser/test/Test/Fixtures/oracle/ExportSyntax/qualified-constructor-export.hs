{- ORACLE_TEST xfail reason="qualified constructor names in export list not handled" -}
{-# LANGUAGE GHC2021 #-}

module QualifiedConstructorExport (
  M.C(M.A)
  ) where

data M = C A | B
data A
