{- ORACLE_TEST
id: expr-s3-do-as-argument
category: expressions
expected: pass
reason: parser supports do as function argument with layout
-}
module X where
x = f (do a;b)
