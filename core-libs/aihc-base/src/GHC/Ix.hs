module GHC.Ix
  ( Ix (..),
    indexError,
  )
where

import Prelude

class (Ord a) => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  unsafeIndex :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int
  unsafeRangeSize :: (a, a) -> Int

  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> hopelessIndexError

  unsafeIndex = index

  rangeSize bounds@(_, upper) =
    case inRange bounds upper of
      True -> unsafeIndex bounds upper + 1
      False -> 0

  unsafeRangeSize bounds@(_, upper) = unsafeIndex bounds upper + 1

indexError :: (Show a) => (a, a) -> a -> String -> b
indexError = indexError

hopelessIndexError :: Int
hopelessIndexError = hopelessIndexError

instance Ix Bool where
  range = enumBounds
  unsafeIndex (lower, _) value = fromEnum value - fromEnum lower
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Bool"
  inRange = enumInRange

instance Ix Ordering where
  range = orderingRange
  unsafeIndex (lower, _) value = orderingIndex value - orderingIndex lower
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Ordering"
  inRange = enumInRangeBy orderingIndex

instance Ix Int where
  range = enumBounds
  unsafeIndex (lower, _) value = value - lower
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Int"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Integer where
  range = enumBounds
  unsafeIndex (lower, _) value = fromInteger (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Integer"
  inRange (lower, upper) value = lower <= value && value <= upper

enumBounds :: (Enum a) => (a, a) -> [a]
enumBounds (lower, upper) = enumFromTo lower upper

enumInRange :: (Enum a) => (a, a) -> a -> Bool
enumInRange = enumInRangeBy fromEnum

enumInRangeBy :: (a -> Int) -> (a, a) -> a -> Bool
enumInRangeBy toIndex (lower, upper) value =
  toIndex lower <= toIndex value && toIndex value <= toIndex upper

orderingRange :: (Ordering, Ordering) -> [Ordering]
orderingRange (LT, LT) = [LT]
orderingRange (LT, EQ) = [LT, EQ]
orderingRange (LT, GT) = [LT, EQ, GT]
orderingRange (EQ, EQ) = [EQ]
orderingRange (EQ, GT) = [EQ, GT]
orderingRange (GT, GT) = [GT]
orderingRange _ = []

orderingIndex :: Ordering -> Int
orderingIndex LT = 0
orderingIndex EQ = 1
orderingIndex GT = 2
