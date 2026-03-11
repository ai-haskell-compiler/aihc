module Game.Mastermind.NonEmptyEnumSet where

import qualified Data.NonEmpty as NonEmpty
import qualified Data.EnumSet as EnumSet
import Data.EnumSet (EnumSet)
import Data.Maybe.HT (toMaybe)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)


newtype T a = Cons {flatten :: EnumSet a}
   deriving (Show)

instance Eq (T a) where
   (==) = equating (EnumSet.enumSetToIntSet . flatten)

instance Ord (T a) where
   compare = comparing (EnumSet.enumSetToIntSet . flatten)


size :: T a -> Int
size = EnumSet.size . flatten

member :: (Enum a) => a -> T a -> Bool
member x = EnumSet.member x . flatten

fromList :: (Enum a) => NonEmpty.T [] a -> T a
fromList = Cons . EnumSet.fromList . NonEmpty.flatten

toFlatList :: (Enum a) => T a -> [a]
toFlatList = EnumSet.toList . flatten

fetch :: EnumSet a -> Maybe (T a)
fetch set = toMaybe (not $ EnumSet.null set) (Cons set)

singleton :: (Enum a) => a -> T a
singleton = Cons . EnumSet.singleton

union :: T a -> T a -> T a
union (Cons a) (Cons b) = Cons $ EnumSet.union a b
