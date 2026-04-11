{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module QualifiedConstructorExport (
  M.C(M.A1, M.A2)
  ) where

import M

