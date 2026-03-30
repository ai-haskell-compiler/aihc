{- ORACLE_TEST
id: decls-data-record-function-type-simple
category: declarations
expected: pass
-}
module M where
data X = X { f :: Int -> Int }
