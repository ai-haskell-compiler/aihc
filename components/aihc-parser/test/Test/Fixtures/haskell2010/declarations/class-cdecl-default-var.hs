{- ORACLE_TEST
id: decls-class-cdecl-default-var
category: declarations
expected: pass
reason: parser now supports default method implementations
-}
module D33 where
class C a where { op :: a -> a; op = id }
