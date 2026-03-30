{- ORACLE_TEST
id: modules-s5-export-qtycon-all
category: modules
expected: pass
reason: parser now supports section 5 wildcard type constructor export entries
-}
module S5ExportQtyConAll (Maybe(..)) where
x = Just 1
