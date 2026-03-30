{- ORACLE_TEST
id: expr-s3-list-multiple
category: expressions
expected: pass
reason: parser supports multi-element list literals
-}
module ExprS307ListMultiple where
x = [1, 2, 3]
