{- ORACLE_TEST
id: expr-s3-expr-type-signature-context
category: expressions
expected: pass
reason: parser now supports expression type signatures with context
-}
module ExprS316TypeSigContext where
x = ((+1) :: Num a => a -> a)
