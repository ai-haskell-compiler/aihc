{- ORACLE_TEST
id: expr-s3-listcomp-let-qualifier
category: expressions
expected: pass
reason: parser now supports list-comprehension let qualifiers
-}
module ExprS311LetQualifier where
x xs = [m | n <- xs, let m = n + 1]
