{- ORACLE_TEST pass -}
module M where
newtype IOMcn a b = IOMcn { getIOMcn :: a -> IO b }