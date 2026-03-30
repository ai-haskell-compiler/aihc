{- ORACLE_TEST
id: pat-irrefutable
category: patterns
expected: pass
reason: parser now supports irrefutable patterns
-}
module P4 where
x ~(a,b) = a + b
