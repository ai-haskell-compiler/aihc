{- ORACLE_TEST pass -}
module ClassDefaultLayout where
class C a where
  op :: a -> a
  op = id