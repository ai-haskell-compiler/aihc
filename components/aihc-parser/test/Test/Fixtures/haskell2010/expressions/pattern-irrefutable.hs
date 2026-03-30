{- ORACLE_TEST
id: expr-s3-pattern-irrefutable
category: expressions
expected: pass
reason: parser now supports irrefutable patterns
-}
module ExprS317PatIrrefutable where
x v = (\ ~(a, b) -> a) v
