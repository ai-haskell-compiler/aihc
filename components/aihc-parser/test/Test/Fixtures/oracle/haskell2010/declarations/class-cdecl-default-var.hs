{- ORACLE_TEST pass -}
module D33 where
class C a where { op :: a -> a; op = id }