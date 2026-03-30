{- ORACLE_TEST
id: expr-s3-arithseq-from-then
category: expressions
expected: pass
reason: parser now supports section 3 arithmetic sequences from-then form
-}
module ExprS310FromThen where
x = [1, 3 ..]
