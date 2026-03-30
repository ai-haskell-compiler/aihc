{- ORACLE_TEST
id: decls-data-infix-strict-left
category: declarations
expected: pass
-}
module D16 where
data T = !Int :*: Bool
