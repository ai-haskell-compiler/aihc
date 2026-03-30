{- ORACLE_TEST
id: expr-s3-arithseq-from-then-to
category: expressions
expected: pass
reason: parser now supports section 3 arithmetic sequences from-then-to form
-}
module ExprS310FromThenTo where
x = [1, 3 .. 10]
