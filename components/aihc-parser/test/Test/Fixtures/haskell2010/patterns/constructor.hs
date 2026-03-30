{- ORACLE_TEST
id: pat-constructor
category: patterns
expected: pass
-}
module P1 where
data T = A Int | B
x (A n) = n
x B = 0
