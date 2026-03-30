{- ORACLE_TEST
id: modules-multiline-definition-export-list-split
category: modules
expected: pass
reason: parser now supports multiline module export lists
-}
module
  MultilineDefinitionExportListSplit
  (x, T(..))
where

x = 1
data T = A | B
