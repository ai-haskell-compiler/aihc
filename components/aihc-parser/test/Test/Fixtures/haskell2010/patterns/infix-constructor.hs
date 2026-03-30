{- ORACLE_TEST
id: pat-infix-constructor
category: patterns
expected: pass
reason: parser now supports infix constructor patterns
-}
module P6 where

headOrZero (x : _) = x
headOrZero [] = 0
