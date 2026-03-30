{- ORACLE_TEST
id: decls-type-synonym
category: declarations
expected: pass
reason: parser now supports type synonym declarations
-}
module D6 where
type Pair a = (a, a)
