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
import GHC.Prim (Int#, compareInt#, (==#))
import GHC.Types (Bool (..), Ordering (..))

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
