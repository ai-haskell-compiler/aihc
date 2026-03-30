{- ORACLE_TEST
id: expr-s3-pattern-tuple
category: expressions
expected: pass
reason: parser now supports tuple patterns
-}
module ExprS317PatTuple where
x t = case t of { (a, b) -> a + b }
