{- ORACLE_TEST
id: expr-s3-lambda-infix-rhs
category: expressions
expected: pass
reason: parser supports lambda as right operand of infix expression
-}
module X where
x = a + \y -> y
