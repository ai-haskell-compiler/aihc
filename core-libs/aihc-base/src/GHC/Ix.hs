module GHC.Ix
  ( Ix (..),
    indexError,
  )
where

import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.Word (Word16, Word32, Word64, Word8)
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

instance (Ix a, Ix b) => Ix (a, b) where
  range ((lowerA, lowerB), (upperA, upperB)) =
    [(a, b) | a <- range (lowerA, upperA), b <- range (lowerB, upperB)]
  unsafeIndex ((lowerA, lowerB), (upperA, upperB)) (a, b) =
    unsafeIndex (lowerA, upperA) a * rangeSize (lowerB, upperB) + unsafeIndex (lowerB, upperB) b
  inRange ((lowerA, lowerB), (upperA, upperB)) (a, b) =
    inRange (lowerA, upperA) a && inRange (lowerB, upperB) b
  unsafeRangeSize ((lowerA, lowerB), (upperA, upperB)) =
    rangeSize (lowerA, upperA) * rangeSize (lowerB, upperB)

instance Ix Int8 where
  range = enumBounds
  unsafeIndex (lower, _) value = fromIntegral (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Int8"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Int16 where
  range = enumBounds
  unsafeIndex (lower, _) value = fromIntegral (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Int16"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Int32 where
  range = enumBounds
  unsafeIndex (lower, _) value = fromIntegral (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Int32"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Int64 where
  range = enumBounds
  unsafeIndex (lower, _) value = fromIntegral (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Int64"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Word where
  range = enumBounds
  unsafeIndex (lower, _) value = fromIntegral (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Word"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Word8 where
  range = enumBounds
  unsafeIndex (lower, _) value = fromIntegral (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Word8"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Word16 where
  range = enumBounds
  unsafeIndex (lower, _) value = fromIntegral (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Word16"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Word32 where
  range = enumBounds
  unsafeIndex (lower, _) value = fromIntegral (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Word32"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Word64 where
  range = enumBounds
  unsafeIndex (lower, _) value = fromIntegral (value - lower)
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Word64"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix Char where
  range = enumBounds
  unsafeIndex (lower, _) value = fromEnum value - fromEnum lower
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "Char"
  inRange (lower, upper) value = lower <= value && value <= upper

instance Ix () where
  range _ = [()]
  unsafeIndex _ _ = 0
  inRange _ _ = True
  index _ _ = 0

instance (Ix a, Ix b, Ix c) => Ix (a, b, c) where
  range ((lowerA, lowerB, lowerC), (upperA, upperB, upperC)) =
    [(a, b, c) | a <- range (lowerA, upperA), b <- range (lowerB, upperB), c <- range (lowerC, upperC)]
  unsafeIndex ((lowerA, lowerB, lowerC), (upperA, upperB, upperC)) (a, b, c) =
    (unsafeIndex (lowerA, upperA) a * rangeSize (lowerB, upperB) + unsafeIndex (lowerB, upperB) b)
      * rangeSize (lowerC, upperC)
      + unsafeIndex (lowerC, upperC) c
  inRange ((lowerA, lowerB, lowerC), (upperA, upperB, upperC)) (a, b, c) =
    inRange (lowerA, upperA) a && inRange (lowerB, upperB) b && inRange (lowerC, upperC) c
  unsafeRangeSize ((lowerA, lowerB, lowerC), (upperA, upperB, upperC)) =
    rangeSize (lowerA, upperA) * rangeSize (lowerB, upperB) * rangeSize (lowerC, upperC)

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
