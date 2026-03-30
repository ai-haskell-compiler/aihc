{- ORACLE_TEST
id: expr-s3-application-left-assoc
category: expressions
expected: pass
reason: parser now supports left-associative application
-}
module ExprS303AppLeftAssoc where
f a b = a + b
x = f 1 2
