{- ORACLE_TEST xfail DEPRECATED pragma in export list not supported by parser -}
module ExportWarningsReexport
  ( {-# DEPRECATED "Import g from ExportWarningsImport instead" #-} g
  ) where

import ExportWarningsImport (g)
