{- ORACLE_TEST pass -}
module ExistentialForall where

toJSON ((f :: f a) :=> (g :: g a)) = ()
