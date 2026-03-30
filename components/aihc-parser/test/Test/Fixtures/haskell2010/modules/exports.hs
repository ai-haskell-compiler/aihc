{- ORACLE_TEST
id: modules-exports
category: modules
expected: pass
reason: parser now supports explicit module export lists
-}
module E (x, T(..)) where
x = 1
data T = A | B
