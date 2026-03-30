{- ORACLE_TEST
id: expr-s3-listcomp-guards
category: expressions
expected: pass
reason: parser now supports simple list-comprehension guard forms
-}
module ExprS311Guards where
x xs = [n | n <- xs, n > 0]
