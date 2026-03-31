{- ORACLE_TEST xfail existential with forall in type -}
module ExistentialForall where

toJSON ((f :: f a) :=> (g :: g a)) = ()
