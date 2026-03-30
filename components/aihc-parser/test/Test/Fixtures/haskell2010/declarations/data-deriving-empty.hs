{- ORACLE_TEST
id: decls-data-deriving-empty
category: declarations
expected: pass
reason: parser now supports empty data deriving clauses
-}
module D23 where
data Phantom a = Phantom deriving ()
