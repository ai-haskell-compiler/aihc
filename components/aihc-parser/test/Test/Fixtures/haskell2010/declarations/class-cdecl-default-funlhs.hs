{- ORACLE_TEST
id: decls-class-cdecl-default-funlhs
category: declarations
expected: pass
reason: parser now supports default method implementations with patterns
-}
module D34 where
class C a where { op :: a -> a; op x = x }
