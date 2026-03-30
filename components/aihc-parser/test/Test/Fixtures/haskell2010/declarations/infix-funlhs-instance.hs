{- ORACLE_TEST
id: decls-infix-funlhs-instance
category: declarations
expected: pass
reason: parser now supports infix definitions in instance declarations
-}
module InfixFunlhsInstance where
data Box a = Box a
instance Eq a => Eq (Box a) where
  Box x == Box y = x == y
