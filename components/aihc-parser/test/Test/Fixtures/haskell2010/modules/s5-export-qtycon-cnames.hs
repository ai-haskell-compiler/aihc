{- ORACLE_TEST
id: modules-s5-export-qtycon-cnames
category: modules
expected: pass
reason: parser now supports section 5 explicit constructor member export entries
-}
module S5ExportQtyConCNames (Maybe(Nothing, Just)) where
x = Just 1
