{- ORACLE_TEST
id: decls-class
category: declarations
expected: pass
-}
module D7 where
class Eq a => Named a where
  nameOf :: a -> String
