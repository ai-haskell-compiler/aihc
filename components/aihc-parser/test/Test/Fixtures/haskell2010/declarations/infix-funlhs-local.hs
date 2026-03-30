{- ORACLE_TEST
id: decls-infix-funlhs-local
category: declarations
expected: pass
reason: parser now supports infix definitions in where clauses
-}
module InfixFunlhsLocal where
f = g
  where
    x <+> y = x + y
    g = 1 <+> 2
