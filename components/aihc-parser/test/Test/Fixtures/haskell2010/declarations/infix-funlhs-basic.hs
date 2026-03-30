{- ORACLE_TEST
id: decls-infix-funlhs-basic
category: declarations
expected: pass
reason: parser now supports infix operator function definitions
-}
module InfixFunlhs where
x <+> y = x + y
