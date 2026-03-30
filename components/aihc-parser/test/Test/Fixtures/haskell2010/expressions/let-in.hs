{- ORACLE_TEST
id: expr-let-in
category: expressions
expected: pass
reason: parser now supports let-in expressions
-}
module X3 where
x n = let y = n + 1 in y * 2
