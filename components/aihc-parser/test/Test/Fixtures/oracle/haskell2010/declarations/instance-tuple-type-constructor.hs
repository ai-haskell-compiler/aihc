{- ORACLE_TEST pass -}
module M where
class Extractable f where
  runSingleton :: f a -> a
instance Extractable ((,,) w s) where
  runSingleton (_,_,x) = x