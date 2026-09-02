{-# LANGUAGE MagicHash #-}

module GHC.Internal.Classes
  ( Eq (..),
    Ord (..),
    Ordering (..),
  )
where

import Data.Bool (not)
import GHC.Classes (Eq (..), Ord (..))
import GHC.Int (Int (..))
import GHC.Internal.Integer (Integer, compareInteger#, eqInteger#)
import GHC.Prim (Int#, Word#, compareInt#, eqWord#, ltWord#, word64ToWord#, word8ToWord#, (==#))
import GHC.Types (Bool (..), Ordering (..))
import GHC.Word (Word (..), Word64 (..), Word8 (..))

instance Eq Bool where
  False == False = True
  False == True = False
  True == False = False
  True == True = True

  left /= right = not (left == right)

instance Eq Int where
  I# left == I# right =
    case (==#) left right of
      0# -> False
      _ -> True

  left /= right = not (left == right)

instance Eq Integer where
  left == right =
    case eqInteger# left right of
      0# -> False
      _ -> True

  left /= right = not (left == right)

instance Eq Ordering where
  LT == LT = True
  EQ == EQ = True
  GT == GT = True
  _ == _ = False

  left /= right = not (left == right)

instance Eq Word where
  W# left == W# right = wordEquals left right
  left /= right = not (left == right)

instance Eq Word8 where
  W8# left == W8# right = wordEquals (word8ToWord# left) (word8ToWord# right)
  left /= right = not (left == right)

instance Eq Word64 where
  W64# left == W64# right = wordEquals (word64ToWord# left) (word64ToWord# right)
  left /= right = not (left == right)

instance Ord Bool where
  compare = compareBool
  left < right = classesLessBy compareBool left right
  left <= right = classesLessOrEqualBy compareBool left right
  left > right = classesGreaterBy compareBool left right
  left >= right = classesGreaterOrEqualBy compareBool left right
  max = classesMaxBy compareBool
  min = classesMinBy compareBool

instance Ord Int where
  compare = compareInt
  left < right = classesLessBy compareInt left right
  left <= right = classesLessOrEqualBy compareInt left right
  left > right = classesGreaterBy compareInt left right
  left >= right = classesGreaterOrEqualBy compareInt left right
  max = classesMaxBy compareInt
  min = classesMinBy compareInt

instance Ord Integer where
  compare = compareInteger
  left < right = classesLessBy compareInteger left right
  left <= right = classesLessOrEqualBy compareInteger left right
  left > right = classesGreaterBy compareInteger left right
  left >= right = classesGreaterOrEqualBy compareInteger left right
  max = classesMaxBy compareInteger
  min = classesMinBy compareInteger

instance Ord Ordering where
  compare = compareOrdering
  left < right = classesLessBy compareOrdering left right
  left <= right = classesLessOrEqualBy compareOrdering left right
  left > right = classesGreaterBy compareOrdering left right
  left >= right = classesGreaterOrEqualBy compareOrdering left right
  max = classesMaxBy compareOrdering
  min = classesMinBy compareOrdering

instance Ord Word where
  compare = compareWord
  left < right = classesLessBy compareWord left right
  left <= right = classesLessOrEqualBy compareWord left right
  left > right = classesGreaterBy compareWord left right
  left >= right = classesGreaterOrEqualBy compareWord left right
  max = classesMaxBy compareWord
  min = classesMinBy compareWord

instance Ord Word8 where
  compare = compareWord8
  left < right = classesLessBy compareWord8 left right
  left <= right = classesLessOrEqualBy compareWord8 left right
  left > right = classesGreaterBy compareWord8 left right
  left >= right = classesGreaterOrEqualBy compareWord8 left right
  max = classesMaxBy compareWord8
  min = classesMinBy compareWord8

instance Ord Word64 where
  compare = compareWord64
  left < right = classesLessBy compareWord64 left right
  left <= right = classesLessOrEqualBy compareWord64 left right
  left > right = classesGreaterBy compareWord64 left right
  left >= right = classesGreaterOrEqualBy compareWord64 left right
  max = classesMaxBy compareWord64
  min = classesMinBy compareWord64

compareBool :: Bool -> Bool -> Ordering
compareBool False False = EQ
compareBool False True = LT
compareBool True False = GT
compareBool True True = EQ

compareInt :: Int -> Int -> Ordering
compareInt (I# left) (I# right) = orderingFromInt# (compareInt# left right)

compareInteger :: Integer -> Integer -> Ordering
compareInteger left right = orderingFromInt# (compareInteger# left right)

compareOrdering :: Ordering -> Ordering -> Ordering
compareOrdering LT LT = EQ
compareOrdering LT _ = LT
compareOrdering EQ LT = GT
compareOrdering EQ EQ = EQ
compareOrdering EQ GT = LT
compareOrdering GT GT = EQ
compareOrdering GT _ = GT

compareWord :: Word -> Word -> Ordering
compareWord (W# left) (W# right) = compareWord# left right

compareWord8 :: Word8 -> Word8 -> Ordering
compareWord8 (W8# left) (W8# right) = compareWord# (word8ToWord# left) (word8ToWord# right)

compareWord64 :: Word64 -> Word64 -> Ordering
compareWord64 (W64# left) (W64# right) = compareWord# (word64ToWord# left) (word64ToWord# right)

wordEquals :: Word# -> Word# -> Bool
wordEquals left right =
  case eqWord# left right of
    0# -> False
    _ -> True

compareWord# :: Word# -> Word# -> Ordering
compareWord# left right =
  case eqWord# left right of
    0# ->
      case ltWord# left right of
        0# -> GT
        _ -> LT
    _ -> EQ

orderingFromInt# :: Int# -> Ordering
orderingFromInt# value =
  case value of
    0# -> EQ
    1# -> GT
    _ -> LT

classesLessBy :: (a -> a -> Ordering) -> a -> a -> Bool
classesLessBy comparison left right =
  case comparison left right of
    LT -> True
    _ -> False

classesLessOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
classesLessOrEqualBy comparison left right =
  case comparison left right of
    GT -> False
    _ -> True

classesGreaterBy :: (a -> a -> Ordering) -> a -> a -> Bool
classesGreaterBy comparison left right =
  case comparison left right of
    GT -> True
    _ -> False

classesGreaterOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
classesGreaterOrEqualBy comparison left right =
  case comparison left right of
    LT -> False
    _ -> True

classesMaxBy :: (a -> a -> Ordering) -> a -> a -> a
classesMaxBy comparison left right =
  case comparison left right of
    GT -> left
    _ -> right

classesMinBy :: (a -> a -> Ordering) -> a -> a -> a
classesMinBy comparison left right =
  case comparison left right of
    GT -> right
    _ -> left
