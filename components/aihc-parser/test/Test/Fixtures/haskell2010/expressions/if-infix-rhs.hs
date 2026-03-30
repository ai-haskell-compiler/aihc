{- ORACLE_TEST
id: expr-s3-if-infix-rhs
category: expressions
expected: pass
reason: parser supports if as right operand of infix expression
-}
module X where
x = a + if b then c else d
