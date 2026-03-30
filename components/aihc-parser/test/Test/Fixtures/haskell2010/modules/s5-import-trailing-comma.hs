{- ORACLE_TEST
id: modules-s5-import-trailing-comma
category: modules
expected: pass
reason: parser now supports section 5 module import entries with trailing commas
-}
module S5ImportTrailingComma where
import Foreign.Ptr (Ptr, )
x = 1
