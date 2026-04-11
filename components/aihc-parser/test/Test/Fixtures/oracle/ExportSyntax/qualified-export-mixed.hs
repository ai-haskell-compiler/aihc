{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}

module QualifiedExportEdgeCases (
  M.T(M.X, M.Y),
  N.C(..)
  ) where

import M
import N

