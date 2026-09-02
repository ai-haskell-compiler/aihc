module GHC.ByteOrder
  ( ByteOrder (..),
    targetByteOrder,
  )
where

import Prelude (Bool (..), Eq (..), Ord (..), Ordering (..), Show (..), showString)

data ByteOrder
  = BigEndian
  | LittleEndian

instance Eq ByteOrder where
  BigEndian == BigEndian = True
  LittleEndian == LittleEndian = True
  _ == _ = False
  left /= right = case left == right of
    True -> False
    False -> True

instance Ord ByteOrder where
  compare BigEndian BigEndian = EQ
  compare BigEndian LittleEndian = LT
  compare LittleEndian BigEndian = GT
  compare LittleEndian LittleEndian = EQ
  left < right = compare left right == LT
  left <= right = compare left right /= GT
  left > right = compare left right == GT
  left >= right = compare left right /= LT
  min left right = case compare left right of
    GT -> right
    _ -> left
  max left right = case compare left right of
    LT -> right
    _ -> left

instance Show ByteOrder where
  showsPrec _ BigEndian = showString "BigEndian"
  showsPrec _ LittleEndian = showString "LittleEndian"

-- | All supported targets are little endian.
targetByteOrder :: ByteOrder
targetByteOrder = LittleEndian
