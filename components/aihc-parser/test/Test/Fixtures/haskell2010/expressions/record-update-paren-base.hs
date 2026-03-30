{- ORACLE_TEST
id: expr-s3-record-update-paren-base
category: expressions
expected: pass
-}
module ExprS315RecordUpdateParenBase where
data R = R { a :: Int }
x r = (f r) { a = 1 }
