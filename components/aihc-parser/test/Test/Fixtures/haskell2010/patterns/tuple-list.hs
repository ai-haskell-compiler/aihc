{- ORACLE_TEST
id: pat-tuple-list
category: patterns
expected: pass
reason: parser now supports tuple and list patterns
-}
module P2 where
x (a,b) = a + b
y (h:t) = h + length t
y [] = 0
