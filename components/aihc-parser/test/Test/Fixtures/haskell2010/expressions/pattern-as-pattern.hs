{- ORACLE_TEST
id: expr-s3-pattern-as-pattern
category: expressions
expected: pass
reason: parser now supports as-patterns in case alternatives
-}
module ExprS317PatAsPattern where
x xs = case xs of { ys@(y:_) -> y; [] -> 0 }
