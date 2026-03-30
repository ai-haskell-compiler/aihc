{- ORACLE_TEST
id: expr-s3-record-construction-fields
category: expressions
expected: pass
-}
module ExprS315RecordConstructionFields where
data R = R { a :: Int, b :: Int }
x = R { a = 1, b = 2 }
