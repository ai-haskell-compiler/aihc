{- ORACLE_TEST xfail infix pattern with type signatures -}
module ExistentialForall where

data a :=> b = (a) :=> (b)

toJSON ((f :: f a) :=> (g :: g a)) = ()
