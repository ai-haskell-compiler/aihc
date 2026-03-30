{- ORACLE_TEST
id: expr-s3-atoms-gcon-tuplecon
category: expressions
expected: pass
reason: parser now supports tuple constructor atoms
-}
module ExprS302GConTuple where
x = (,,)
