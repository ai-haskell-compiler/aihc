{- ORACLE_TEST
id: modules-export-var-operator
category: modules
expected: pass
reason: parser now supports parenthesized operator export entries
-}
module Test ((</$>)) where
(</$>) = undefined
