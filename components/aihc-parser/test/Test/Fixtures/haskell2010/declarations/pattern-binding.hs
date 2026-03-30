{- ORACLE_TEST
id: decls-pattern-binding
category: declarations
expected: pass
reason: parser now supports pattern binding declarations
-}
module D3 where
(x, y) = (1, 2)
