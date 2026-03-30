{- ORACLE_TEST
id: expr-s3-errors-error
category: expressions
expected: pass
reason: parser now handles error application expression case
-}
module ExprS301Error where
x = error "boom"
