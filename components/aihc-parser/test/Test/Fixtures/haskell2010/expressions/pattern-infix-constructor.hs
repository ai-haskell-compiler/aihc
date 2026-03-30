{- ORACLE_TEST
id: expr-s3-pattern-infix-constructor
category: expressions
expected: pass
reason: parser now supports infix constructor patterns in case alternatives
-}
module ExprS317PatInfixConstructor where
x xs = case xs of { y:ys -> y; [] -> 0 }
