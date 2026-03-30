{- ORACLE_TEST
id: expr-s3-parenthesized-expression
category: expressions
expected: pass
reason: parser now supports parenthesized expressions
-}
module ExprS309Paren where
x = (1 + 2)
