module Sound.MIDI.Monoid where

import Data.Foldable (foldMap, )
import Data.Monoid (Monoid, mappend, )
import Data.Semigroup (Semigroup, sconcat, )
import Data.List.NonEmpty (NonEmpty, )



infixr 5 +#+

(+#+) :: Monoid m => m -> m -> m
(+#+) = mappend


genAppend :: (Monoid m) =>
   (m -> a) -> (a -> m) -> a -> a -> a
genAppend cons decons x y =
   cons $ mappend (decons x) (decons y)

genConcat :: (Monoid m) =>
   (m -> a) -> (a -> m) -> [a] -> a
genConcat cons decons =
   cons . foldMap decons

nonEmptyConcat :: (Semigroup m) =>
   (m -> a) -> (a -> m) -> NonEmpty a -> a
nonEmptyConcat cons decons =
   cons . sconcat . fmap decons
