{- ORACLE_TEST
id: gadt-existential
category: declarations
expected: pass
-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}

module GadtExistential where

-- Existential with traditional syntax
data Foo = forall a. MkFoo a (a -> Bool)

-- Same thing with GADT syntax
data Foo' where
  MkFoo' :: a -> (a -> Bool) -> Foo'

-- Multiple constructors including nullary
data Bar where
   MkBar :: a -> (a -> Bool) -> Bar
   Nil   :: Bar
