{- ORACLE_TEST pass -}
module D34 where
class C a where { op :: a -> a; op x = x }