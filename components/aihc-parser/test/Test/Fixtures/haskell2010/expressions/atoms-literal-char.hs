{- ORACLE_TEST
id: expr-s3-atoms-literal-char
category: expressions
expected: pass
reason: parser now handles char literal atom case
-}
module ExprS302LiteralChar where
x = 'a'
