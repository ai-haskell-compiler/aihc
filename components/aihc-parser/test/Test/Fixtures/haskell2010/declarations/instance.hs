{- ORACLE_TEST
id: decls-instance
category: declarations
expected: pass
reason: parser now supports infix operator function definitions in instance declarations
-}
module D8 where
data Box a = Box a
instance Eq a => Eq (Box a) where
  Box x == Box y = x == y
