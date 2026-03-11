module Game.Mastermind.CodeSet (
   C(..),
   cube,
   unions,
   intersections,
   intersectionsPQ,
   (*&), (#*&),
   ) where

import qualified Game.Mastermind.NonEmptyEnumSet as NonEmptySet

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import Data.EnumSet (EnumSet)
import Data.Function.HT (nest, )
import Data.Ord.HT (comparing, )

import Prelude hiding (null, )


class C set where
   empty :: set a
   union, intersection :: (Enum a) => set a -> set a -> set a
   unit :: set a
   leftNonEmptyProduct :: NonEmptySet.T a -> set a -> set a
   flatten :: (Enum a) => set a -> [[a]]
   symbols :: (Enum a) => set a -> EnumSet a
   null :: set a -> Bool
   size :: set a -> Integer
   select :: (Enum a) => set a -> Integer -> [a]
   representationSize :: set a -> Int
   -- | simplify set representation by combining set products where possible
   compress :: (Enum a) => set a -> set a

cube :: (C set) => NonEmptySet.T a -> Int -> set a
cube alphabet n =
   nest n (leftNonEmptyProduct alphabet) unit

unions :: (C set, Enum a) => [set a] -> set a
unions = foldr union empty


{-
Use a sorted list as a priority queue.

Using the set 'size' as priority would be an unnecessary effort.
Intersection makes sets smaller,
thus the procedure would always insert at the front.
This is what 'intersections' does anyway.
-}
intersectionsPQ :: (C set, Enum a) => NonEmpty.T [] (set a) -> set a
intersectionsPQ =
   let go (NonEmpty.Cons (_, set) []) = set
       go (NonEmpty.Cons (_,x) ((_,y):rest)) =
          let sec = intersection x y
          in  go $
              NonEmpty.insertBy
                  (comparing fst) (representationSize sec, sec) rest
   in  go .
       NonEmptyC.sortBy (comparing fst) .
       fmap (\set -> (representationSize set, set))

intersections :: (C set, Enum a) => NonEmpty.T [] (set a) -> set a
intersections = NonEmpty.foldl1 intersection . nonEmptySortKey size

-- cannot be easily generalized for inclusion in non-empty package
nonEmptySortKey :: (Ord b) => (a -> b) -> NonEmpty.T [] a -> NonEmpty.T [] a
nonEmptySortKey f =
   fmap snd . NonEmptyC.sortBy (comparing fst) . fmap (\x -> (f x, x))


infixr 5 *&, #*&

{- |
Like 'leftNonEmptyProduct' but the left operand can be empty.
-}
(*&) :: (C set, Enum a) => EnumSet a -> set a -> set a
c *& set =
   case NonEmptySet.fetch c of
      Nothing -> empty
      Just nec -> leftNonEmptyProduct nec set

(#*&) :: (C set, Enum a) => a -> set a -> set a
c #*& set = leftNonEmptyProduct (NonEmptySet.singleton c) set
