{- ORACLE_TEST
id: lexical-numeric-literals
category: lexical
expected: pass
reason: parser now handles numeric literals in lexical suite
-}
module K3 where
x = 10
y = 0x10
z = 3.14
