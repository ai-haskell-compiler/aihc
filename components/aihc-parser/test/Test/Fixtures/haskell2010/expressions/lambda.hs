{- ORACLE_TEST
id: expr-lambda
category: expressions
expected: pass
reason: parser now supports lambda expressions
-}
module X4 where
x = \n -> n + 1
