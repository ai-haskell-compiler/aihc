{- ORACLE_TEST
id: decls-instance-method-guards
category: declarations
expected: pass
reason: parser now supports guards in instance method definitions
-}
module M where
class Class a where
  fn :: a -> ()
instance Class X where
  fn a | True = ()
