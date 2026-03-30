{- ORACLE_TEST
id: expr-s3-operators-conop-colon
category: expressions
expected: pass
reason: parser now supports infix constructor operators in expressions
-}
module ExprS302ConopColon where
x = 1 : []
