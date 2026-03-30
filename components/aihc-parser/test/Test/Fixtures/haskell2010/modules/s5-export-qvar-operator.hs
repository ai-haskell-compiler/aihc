{- ORACLE_TEST
id: modules-s5-export-qvar-operator
category: modules
expected: pass
reason: parser now supports section 5 parenthesized operator export entries
-}
module S5ExportQVarOperator ((</$>)) where
(</$>) = const
