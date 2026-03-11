{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Lawful (Lawful) where

-- | A `Lawful c t` instance asserts that the typeclass `c t`
--   obeys the expected laws of `c` (whatever that means).
class c t => Lawful c t

---------------------------------------------------------------------
--  Some lawful instances of `Eq`, `Ord`

-- Base types
instance Lawful Eq  Bool
instance Lawful Ord Bool

instance Lawful Eq  Int
instance Lawful Ord Int

instance Lawful Eq  Integer
instance Lawful Ord Integer

-- Lists
instance Lawful Eq  a => Lawful Eq [a]
instance Lawful Ord a => Lawful Ord [a]

-- Maybe
instance Lawful Eq  a => Lawful Eq  (Maybe a)
instance Lawful Ord a => Lawful Ord (Maybe a)

-- Either
instance (Lawful Eq  a, Lawful Eq  b) => Lawful Eq  (Either a b)
instance (Lawful Ord a, Lawful Ord b) => Lawful Ord (Either a b)
