{- ORACLE_TEST
id: expr-s3-do-infix-rhs
category: expressions
expected: pass
reason: parser supports do as right operand of infix expression
-}
module X where
x = y <$ do a;b
