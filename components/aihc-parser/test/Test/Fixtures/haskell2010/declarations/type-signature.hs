{- ORACLE_TEST
id: decls-type-signature
category: declarations
expected: pass
reason: parser now supports top-level type signatures
-}
module D1 where
idInt :: Int -> Int
idInt x = x
