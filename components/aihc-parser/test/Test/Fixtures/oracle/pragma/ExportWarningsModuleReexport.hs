{- ORACLE_TEST xfail DEPRECATED pragma in export list not supported by parser -}
module ExportWarningsModuleReexport
  ( {-# DEPRECATED "This declaration has moved to ExportWarningsModuleSource" #-}
      module ExportWarningsModuleSource
  ) where

import ExportWarningsModuleSource
