module Data.NonEmpty.Foldable where

import qualified Data.Foldable as Fold
import Data.Foldable (Foldable, )


{- |
It holds:

> foldMap f . Mapped g = foldMap f . fmap g

but use of 'Mapped' avoids 'Functor' constraint.
-}
data Mapped f a b = Mapped (a -> b) (f a)


instance (Foldable f) => Foldable (Mapped f a) where
   foldMap g (Mapped f xs) = Fold.foldMap (g . f) xs
   foldr g x (Mapped f xs) = Fold.foldr (g . f) x xs
   foldl g x (Mapped f xs) = Fold.foldl (\acc -> g acc . f) x xs
