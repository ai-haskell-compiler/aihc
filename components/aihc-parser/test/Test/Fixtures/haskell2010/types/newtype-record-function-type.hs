{- ORACLE_TEST
id: types-newtype-record-function-type
category: types
expected: pass
-}
module M where
newtype IOMcn a b = IOMcn { getIOMcn :: a -> IO b }
