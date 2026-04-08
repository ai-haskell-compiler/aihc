{- ORACLE_TEST pass -}
module ExportWarningsModuleReexport
  ( {-# DEPRECATED "This declaration has moved to ExportWarningsModuleSource" #-}
      module ExportWarningsModuleSource
  ) where

import ExportWarningsModuleSource
