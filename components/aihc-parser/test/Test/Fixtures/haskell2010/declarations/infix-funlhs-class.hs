{- ORACLE_TEST
id: decls-infix-funlhs-class
category: declarations
expected: pass
reason: parser now supports infix definitions in class declarations
-}
module InfixFunlhsClass where
class MyClass a where
  x <+> y = undefined
