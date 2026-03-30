{- ORACLE_TEST
id: expr-s3-record-update-multiple
category: expressions
expected: pass
-}
module ExprS315RecordUpdateMultiple where
data R = R { a :: Int, b :: Int }
x r = r { a = 3, b = 4 }
