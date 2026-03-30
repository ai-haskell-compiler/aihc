{- ORACLE_TEST
id: decls-class-cdecl-signature-context
category: declarations
expected: pass
-}
module D32 where
class C a where { op :: Num b => a -> b -> a }
