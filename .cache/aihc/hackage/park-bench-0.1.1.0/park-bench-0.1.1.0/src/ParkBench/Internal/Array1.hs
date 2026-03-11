module ParkBench.Internal.Array1
  ( Array1,
    singleton,
    fromList,
    unsafeFromList,
    fromList1,
    toList,
    toList1,
    get,
    uncons,
    mapMaybe,
  )
where

import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List1
import qualified Data.Maybe as Maybe
import ParkBench.Internal.Prelude

-- | A non-empty array.
data Array1 a
  = Array1 a (Array Int a)
  deriving stock (Foldable, Functor, Traversable)

-- | Make a singleton non-empty array.
singleton :: a -> Array1 a
singleton x =
  Array1 x (Array.array (1, 0) []) -- bogus bounds is the only way to make an empty array LOL

-- | Convert a list into a non-empty array.
fromList :: [a] -> Maybe (Array1 a)
fromList = \case
  [] -> Nothing
  x : xs -> Just (fromUncons x xs)

-- | Convert a list into a non-empty array. Fails if the given list is empty.
unsafeFromList :: [a] -> Array1 a
unsafeFromList = \case
  [] -> error "Array1.unsafeFromList: empty list"
  x : xs -> fromUncons x xs

-- | Convert a non-empty list into a non-empty array.
fromList1 :: List.NonEmpty a -> Array1 a
fromList1 (x List1.:| xs) =
  fromUncons x xs

fromUncons :: a -> [a] -> Array1 a
fromUncons x xs =
  Array1 x (Array.listArray (0, Foldable.length xs - 1) xs)

-- | Convert a non-empty array into a list.
toList :: Array1 a -> [a]
toList =
  Foldable.toList

-- | Convert a non-empty array into a non-empty list.
toList1 :: Array1 a -> List.NonEmpty a
toList1 (Array1 x xs) =
  x List1.:| Foldable.toList xs

-- | Get an element from a non-empty array.
get :: Int -> Array1 a -> a
get i (Array1 x xs) =
  if i == 0 then x else xs Array.! (i - 1)

-- | Convert a non-empty array into a head element and tail array.
uncons :: Array1 a -> (a, Array Int a)
uncons (Array1 x xs) =
  (x, xs)

mapMaybe :: (a -> Maybe b) -> Array1 a -> Maybe (Array1 b)
mapMaybe f =
  fromList . Maybe.mapMaybe f . Foldable.toList
