{- ORACLE_TEST pass -}
module D32 where
class C a where { op :: Num b => a -> b -> a }