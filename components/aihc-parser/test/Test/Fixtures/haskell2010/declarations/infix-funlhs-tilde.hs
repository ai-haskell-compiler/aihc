{- ORACLE_TEST
id: decls-infix-funlhs-tilde
category: declarations
expected: pass
reason: whitespace-sensitive ~ as infix operator (GHC proposal 0229)
-}
module InfixFunlhsTilde where
x ~ y = ()
