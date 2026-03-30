{- ORACLE_TEST
id: expr-s3-do-in-list
category: expressions
expected: pass
reason: parser supports do inside list brackets with layout
-}
module X where
x = [do a;b]
