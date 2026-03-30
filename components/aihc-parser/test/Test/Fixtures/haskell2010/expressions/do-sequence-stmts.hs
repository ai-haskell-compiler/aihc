{- ORACLE_TEST
id: expr-s3-do-sequence-stmts
category: expressions
expected: pass
reason: parser now supports do statement sequencing
-}
module ExprS314DoSequenceStmts where
x = do { a <- Just 1; b <- Just 2; return (a + b) }
