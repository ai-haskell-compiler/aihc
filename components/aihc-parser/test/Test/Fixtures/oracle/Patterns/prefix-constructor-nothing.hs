{- ORACLE_TEST pass -}
module PrefixConstructorNothing where

data T = T (Maybe Int) (Maybe Int)

f (T Nothing Nothing) = ()
f _ = ()
