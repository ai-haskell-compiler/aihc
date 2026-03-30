{- ORACLE_TEST
id: decls-data-prefix-strict
category: declarations
expected: pass
-}
module D14 where
data Pair a = Pair !a a
