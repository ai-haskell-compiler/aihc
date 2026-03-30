{- ORACLE_TEST
id: pat-literal-wildcard-parenthesized
category: patterns
expected: pass
reason: parser now supports literal, wildcard, and parenthesized patterns
-}
module P9 where

isA 'a' = True
isA _ = False

unwrap ((x)) = x
