{- ORACLE_TEST
id: modules-s5-module-empty-exports
category: modules
expected: pass
reason: parser now supports explicit empty module export lists
-}
module S5ModuleEmptyExports () where
x = 1
