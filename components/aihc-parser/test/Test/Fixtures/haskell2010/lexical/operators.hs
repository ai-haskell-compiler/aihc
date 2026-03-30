{- ORACLE_TEST
id: lexical-operators
category: lexical
expected: pass
reason: parser now handles lexical operator forms
-}
module K4 where
x = (1 + 2) * 3
y = 3 `div` 2
