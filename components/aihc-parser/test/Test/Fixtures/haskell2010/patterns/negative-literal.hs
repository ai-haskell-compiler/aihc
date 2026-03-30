{- ORACLE_TEST
id: pat-negative-literal
category: patterns
expected: pass
reason: parser now supports negative literal patterns
-}
module P7 where

intSign (-1) = 0
intSign n = n

floatSign (-1.0) = 0.0
floatSign x = x
