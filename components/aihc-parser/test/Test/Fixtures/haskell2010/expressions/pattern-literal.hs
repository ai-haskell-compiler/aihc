{- ORACLE_TEST
id: expr-s3-pattern-literal
category: expressions
expected: pass
reason: parser now supports literal patterns
-}
module ExprS317PatLiteral where
x n = case n of { 1 -> 1; _ -> 0 }
