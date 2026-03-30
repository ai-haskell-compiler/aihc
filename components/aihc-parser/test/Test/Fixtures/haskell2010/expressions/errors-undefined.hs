{- ORACLE_TEST
id: expr-s3-errors-undefined
category: expressions
expected: pass
reason: parser now handles this expression atom case
-}
module ExprS301Undefined where
x = undefined
