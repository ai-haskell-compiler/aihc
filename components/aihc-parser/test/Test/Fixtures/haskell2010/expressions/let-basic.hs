{- ORACLE_TEST
id: expr-s3-let-basic
category: expressions
expected: pass
reason: parser now supports basic let expressions
-}
module ExprS312LetBasic where
x = let y = 1 in y
