{- ORACLE_TEST
id: expr-sections
category: expressions
expected: pass
reason: parser now supports operator sections
-}
module X6 where
x = map (+1) [1,2,3]
y = map (1+) [1,2,3]
