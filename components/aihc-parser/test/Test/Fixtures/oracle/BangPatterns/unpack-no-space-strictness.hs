{- ORACLE_TEST pass -}
data Pair a = Null | Cons a {-# UNPACK #-}!(IORef (Pair a))
