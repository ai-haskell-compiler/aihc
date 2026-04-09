{- ORACLE_TEST xfail UNPACK pragma followed immediately by strictness bang without space -}
data Pair a = Null | Cons a {-# UNPACK #-}!(IORef (Pair a))
