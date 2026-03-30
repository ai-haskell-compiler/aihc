{- ORACLE_TEST
id: expr-where-clause
category: expressions
expected: pass
reason: parser now supports where clauses in declarations
-}
module X8 where
x n = y + 1 where y = n * 2
