{- ORACLE_TEST
id: expr-s3-pattern-nullary-constructor
category: expressions
expected: pass
reason: parser now handles nullary constructor case alternatives
-}
module ExprS317PatNullaryConstructor where
data D = C
x d = case d of { C -> 1 }
