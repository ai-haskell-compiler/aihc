{- ORACLE_TEST
id: decls-class-cdecl-multiple-defaults
category: declarations
expected: pass
reason: parser supports multiple default methods with blank line
-}
module ClassMultipleDefaults where
class C a where
  foo :: a -> a
  foo = id

  bar :: a -> Int
  bar x = 42
