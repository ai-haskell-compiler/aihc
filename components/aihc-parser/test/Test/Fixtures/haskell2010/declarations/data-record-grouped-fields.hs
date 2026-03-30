{- ORACLE_TEST
id: decls-data-record-grouped-fields
category: declarations
expected: pass
-}
module D20 where
data Point = Point { x, y :: Int }
