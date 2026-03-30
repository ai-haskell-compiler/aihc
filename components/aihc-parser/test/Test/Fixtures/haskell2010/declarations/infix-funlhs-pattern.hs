{- ORACLE_TEST
id: decls-infix-funlhs-pattern
category: declarations
expected: pass
reason: parser now supports infix definitions with patterns
-}
module InfixFunlhsPattern where
(Just a) <||> _ = Just a
Nothing <||> b = b
