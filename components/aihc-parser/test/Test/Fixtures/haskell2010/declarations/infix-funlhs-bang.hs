{- ORACLE_TEST
id: decls-infix-funlhs-bang
category: declarations
expected: pass
reason: whitespace-sensitive lexing (GHC proposal 0229)
-}
module InfixFunlhsBang where
x ! y = ()
