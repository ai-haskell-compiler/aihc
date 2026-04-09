{- ORACLE_TEST xfail Unicode type variable in class declaration head fails to parse -}
{-# LANGUAGE UnicodeSyntax #-}

-- Unicode type variables in class heads are not handled correctly.

module UnicodeClassTypeVar where

class Foo α where
  foo :: α
