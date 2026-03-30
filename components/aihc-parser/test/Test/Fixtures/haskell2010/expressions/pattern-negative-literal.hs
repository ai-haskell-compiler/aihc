{- ORACLE_TEST
id: expr-s3-pattern-negative-literal
category: expressions
expected: pass
reason: parser now supports negative literal patterns
-}
module ExprS317PatNegativeLiteral where
x n = case n of { -1 -> 1; _ -> 0 }
