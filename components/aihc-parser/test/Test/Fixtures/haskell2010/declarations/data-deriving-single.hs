{- ORACLE_TEST
id: decls-data-deriving-single
category: declarations
expected: pass
reason: parser now supports data deriving clauses
-}
module D22 where
data Flag = On | Off deriving Eq
