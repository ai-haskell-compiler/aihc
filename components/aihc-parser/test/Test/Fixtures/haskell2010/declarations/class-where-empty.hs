{- ORACLE_TEST
id: decls-class-where-empty
category: declarations
expected: pass
reason: parser now supports empty class bodies with explicit braces
-}
module D26 where
class C a where {}
