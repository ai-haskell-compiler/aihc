{- ORACLE_TEST
id: expr-s3-case-infix-rhs
category: expressions
expected: pass
reason: parser supports case as right operand of infix expression
-}
module X where
x = a + case b of { c -> d }
