{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Generator (SeedLike (..), Frozen (..)) where

import Data.Typeable (Typeable)

-- | A class with a Typeable superclass. An instance for an applied type
-- builds its Typeable evidence from the class's selector, which the
-- instance module never imports.
class Typeable g => SeedLike g where
  seedSize :: g -> Int

-- | A class whose associated family names the mutable form of a value. A
-- method that matches on the constructor of that form has a scrutinee of
-- the family type, not of the newtype.
class Frozen f m where
  type Mutable f m
  freeze :: Mutable f m -> m f
  modify :: Mutable f m -> (f -> f) -> m (Mutable f m)
