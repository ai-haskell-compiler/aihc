{- ORACLE_TEST
id: expr-s3-minus-with-parenthesized-negation
category: expressions
expected: pass
reason: parser now supports parenthesized prefix-negation forms
-}
module ExprS304MinusParenNeg where
x = 1 - (-2)
