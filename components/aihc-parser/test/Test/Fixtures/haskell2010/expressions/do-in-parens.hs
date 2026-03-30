{- ORACLE_TEST
id: expr-s3-do-in-parens
category: expressions
expected: pass
reason: parser supports do inside parentheses with layout
-}
module X where
x = (do a;b)
