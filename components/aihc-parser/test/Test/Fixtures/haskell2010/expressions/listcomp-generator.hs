{- ORACLE_TEST
id: expr-s3-listcomp-generator
category: expressions
expected: pass
reason: parser now supports simple list-comprehension generator forms
-}
module ExprS311Generator where
x xs = [n | n <- xs]
