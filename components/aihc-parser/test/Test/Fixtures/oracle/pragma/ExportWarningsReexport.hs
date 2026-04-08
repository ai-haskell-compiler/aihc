{- ORACLE_TEST pass -}
module ExportWarningsReexport
  ( {-# DEPRECATED "Import g from ExportWarningsImport instead" #-} g
  ) where

import ExportWarningsImport (g)
