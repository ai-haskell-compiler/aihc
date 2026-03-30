{- ORACLE_TEST
id: expr-s3-let-infix-rhs
category: expressions
expected: pass
reason: parser supports let as right operand of infix expression
-}
module X where
x = a + let y = z in y
