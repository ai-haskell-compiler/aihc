{- ORACLE_TEST
id: decls-infix-funlhs-let
category: declarations
expected: pass
reason: parser now supports infix definitions in let expressions
-}
module InfixFunlhsLet where
f = let x <+> y = x + y in 1 <+> 2
