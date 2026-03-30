{- ORACLE_TEST
id: expr-s3-do-let-stmt
category: expressions
expected: pass
reason: parser now supports do-notation let statements
-}
module ExprS314DoLetStmt where
x = do { let { n = 1 }; return n }
