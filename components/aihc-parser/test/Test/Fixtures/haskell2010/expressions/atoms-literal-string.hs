{- ORACLE_TEST
id: expr-s3-atoms-literal-string
category: expressions
expected: pass
reason: parser now handles string literal atom case
-}
module ExprS302LiteralString where
x = "abc"
