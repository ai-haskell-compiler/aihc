{- ORACLE_TEST
id: expr-s3-pattern-list
category: expressions
expected: pass
reason: parser now supports list patterns
-}
module ExprS317PatList where
x xs = case xs of { [a, b] -> a + b; _ -> 0 }
