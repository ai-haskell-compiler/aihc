{- ORACLE_TEST
id: expr-arithmetic-seq
category: expressions
expected: pass
reason: parser now supports arithmetic sequence expressions
-}
module X10 where
x = [1,3..21]
y = [1..10]
