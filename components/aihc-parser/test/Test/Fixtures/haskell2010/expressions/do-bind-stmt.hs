{- ORACLE_TEST
id: expr-s3-do-bind-stmt
category: expressions
expected: pass
reason: parser now supports do bind statements
-}
module ExprS314DoBindStmt where
x = do { n <- Just 1; return n }
