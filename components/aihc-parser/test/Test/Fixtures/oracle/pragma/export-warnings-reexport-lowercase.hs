{- ORACLE_TEST xfail WarningText does not carry pragmaRawText so the pretty-printer normalises to {-# DEPRECATED #-} -}
module ExportWarningsReexportLowercase
  ( {-# deprecated "Import g from ExportWarningsImport instead" #-} g
  ) where

import ExportWarningsImport (g)
