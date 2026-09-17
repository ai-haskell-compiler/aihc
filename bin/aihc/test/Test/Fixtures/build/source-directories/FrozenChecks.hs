{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module FrozenChecks (frozenChecks) where

import Generator

newtype Counter = Counter Int

instance SeedLike Counter where
  seedSize (Counter n) = n

newtype Boxed g = Boxed g

-- | The superclass evidence is @Typeable (Boxed g)@, built from @Typeable g@.
instance SeedLike g => SeedLike (Boxed g) where
  seedSize (Boxed g) = seedSize g + 1

newtype Handle g = Handle g

-- | The family has two arguments and the newtype one, so a coercion built
-- from the family's arguments has the wrong arity.
instance Frozen (Boxed g) Maybe where
  type Mutable (Boxed g) Maybe = Handle g
  freeze (Handle g) = Just (Boxed g)
  modify (Handle g) f =
    case f (Boxed g) of
      Boxed g' -> Just (Handle g')

bump :: Boxed Counter -> Boxed Counter
bump (Boxed (Counter n)) = Boxed (Counter (n + 1))

frozenChecks :: Bool
frozenChecks =
  case (modify (Handle (Counter 1)) bump :: Maybe (Handle Counter)) >>= freeze of
    Just (Boxed (Counter n)) -> n == 2 && seedSize (Boxed (Counter n)) == 3
    Nothing -> False
