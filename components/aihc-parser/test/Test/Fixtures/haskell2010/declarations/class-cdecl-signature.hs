{- ORACLE_TEST
id: decls-class-cdecl-signature
category: declarations
expected: pass
-}
module D31 where
class C a where { op :: a -> a }
