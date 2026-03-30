{- ORACLE_TEST
id: modules-s5-export-qtycls-methods
category: modules
expected: pass
reason: parser now supports section 5 explicit class/type member export entries
-}
module S5ExportQtyClsMethods (Ord(compare)) where
x = EQ
