{- ORACLE_TEST
id: decls-data-infix
category: declarations
expected: pass
-}
module D15 where
data Pair a = a :*: a
