{- ORACLE_TEST
id: decls-infix-funlhs-with-sig
category: declarations
expected: pass
reason: parser now supports infix definitions with type signatures
-}
module InfixFunlhsWithSig where
(<+>) :: Int -> Int -> Int
x <+> y = x + y
