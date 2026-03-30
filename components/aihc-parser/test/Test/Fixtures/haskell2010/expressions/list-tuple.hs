{- ORACLE_TEST
id: expr-list-tuple
category: expressions
expected: pass
reason: parser now supports list and tuple expressions
-}
module X5 where
x = ([1,2,3], (1,2,3))
