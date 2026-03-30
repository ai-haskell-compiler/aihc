{- ORACLE_TEST
id: decls-class-cdecl-default-layout
category: declarations
expected: pass
reason: parser supports layout-based default method implementations
-}
module ClassDefaultLayout where
class C a where
  op :: a -> a
  op = id
