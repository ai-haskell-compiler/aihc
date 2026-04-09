{- ORACLE_TEST pass -}
{-# LANGUAGE UnicodeSyntax #-}

-- Unicode type variables in class heads are now handled correctly.

module UnicodeClassTypeVar where

class Foo α where
  foo :: α
