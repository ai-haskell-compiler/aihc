{- ORACLE_TEST
id: modules-multiline-definition-export-list-items-split
category: modules
expected: pass
reason: parser now supports multiline module export list items
-}
module MultilineDefinitionExportListItemsSplit
  ( x
  , T(..)
  )
  where

x = 1
data T = A | B
