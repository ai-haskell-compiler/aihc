{- ORACLE_TEST
id: decls-deriving
category: declarations
expected: pass
reason: parser now supports parenthesized data deriving clauses
-}
module D11 where
data Mode = Fast | Slow deriving (Eq, Show)
