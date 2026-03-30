{- ORACLE_TEST
id: expr-s3-lambda-single-apat
category: expressions
expected: pass
reason: parser now supports single-pattern lambda forms
-}
module ExprS303LambdaSingle where
x = (\n -> n) 1
