{- ORACLE_TEST
id: expr-s3-record-field-selection
category: expressions
expected: pass
-}
module ExprS315FieldSelection where
data R = R { a :: Int }
x r = a r
