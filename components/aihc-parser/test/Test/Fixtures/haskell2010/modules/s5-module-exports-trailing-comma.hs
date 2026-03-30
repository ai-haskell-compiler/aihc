{- ORACLE_TEST
id: modules-s5-module-exports-trailing-comma
category: modules
expected: pass
reason: parser now supports section 5 module export entries with trailing commas
-}
module S5ModuleExportsTrailingComma (x,) where
x = 1
