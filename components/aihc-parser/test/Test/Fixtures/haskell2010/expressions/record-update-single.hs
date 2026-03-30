{- ORACLE_TEST
id: expr-s3-record-update-single
category: expressions
expected: pass
-}
module ExprS315RecordUpdateSingle where
data R = R { a :: Int, b :: Int }
x r = r { a = 3 }
