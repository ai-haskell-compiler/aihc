{- ORACLE_TEST
id: decls-class-cdecl-fixity
category: declarations
expected: pass
reason: parser now supports class fixity declarations
-}
module D35 where
class C a where { (<+>) :: a -> a -> a; infixl 6 <+> }
