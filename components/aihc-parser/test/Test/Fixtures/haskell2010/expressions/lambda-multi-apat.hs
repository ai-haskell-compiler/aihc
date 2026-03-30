{- ORACLE_TEST
id: expr-s3-lambda-multi-apat
category: expressions
expected: pass
reason: parser now supports multi-pattern lambda forms
-}
module ExprS303LambdaMulti where
x = (\a b -> a + b) 1 2
