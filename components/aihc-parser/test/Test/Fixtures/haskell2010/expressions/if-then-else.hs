{- ORACLE_TEST
id: expr-if-then-else
category: expressions
expected: pass
reason: parser now handles this if-then-else form
-}
module X1 where
x = if True then 1 else 0
