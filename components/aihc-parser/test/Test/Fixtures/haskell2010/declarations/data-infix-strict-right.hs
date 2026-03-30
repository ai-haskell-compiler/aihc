{- ORACLE_TEST
id: decls-data-infix-strict-right
category: declarations
expected: pass
reason: parser now supports infix data constructors with strict right arguments
-}
module D17 where
data T = Int :*: !Bool
