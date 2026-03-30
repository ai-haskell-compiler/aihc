{- ORACLE_TEST
id: decls-data-record-fields
category: declarations
expected: pass
-}
module D19 where
data Person = Person { name :: String, age :: Int }
