{- ORACLE_TEST
id: expr-s3-do-single-expression
category: expressions
expected: pass
reason: parser now supports single-expression do blocks
-}
module ExprS314DoSingleExpression where
x = do { Just 1 }
