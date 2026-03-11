module Test.DocTest.Location where

import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)


-- | A thing with a location attached.
data Located pos a = Located pos a
  deriving (Eq, Show)

instance Functor (Located pos) where
  fmap f (Located loc a) = Located loc $ f a

instance Foldable (Located pos) where
  foldMap f (Located _loc a) = f a

instance Traversable (Located pos) where
  traverse f (Located loc a) = fmap (Located loc) $ f a


-- | Discard location information.
unLoc :: Located pos a -> a
unLoc (Located _ a) = a
