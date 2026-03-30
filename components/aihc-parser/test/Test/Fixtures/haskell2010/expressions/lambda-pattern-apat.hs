{- ORACLE_TEST
id: expr-s3-lambda-pattern-apat
category: expressions
expected: pass
reason: parser now supports non-trivial lambda patterns
-}
module ExprS303LambdaPattern where
x = (\(a, b) -> a) (1, 2)
