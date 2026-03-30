{- ORACLE_TEST
id: expr-s3-listcomp-multi-generator
category: expressions
expected: pass
reason: parser now supports multiple list-comprehension generators
-}
module ExprS311MultiGenerator where
x xs ys = [(a, b) | a <- xs, b <- ys]
