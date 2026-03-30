{- ORACLE_TEST
id: pat-as-pattern
category: patterns
expected: pass
reason: parser now supports as-patterns
-}
module P3 where
x s@(h:_) = (h, s)
x [] = ('_', [])
