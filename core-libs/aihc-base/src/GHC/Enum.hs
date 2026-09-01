{-# LANGUAGE MagicHash #-}

module GHC.Enum
  ( Bounded (..),
    Enum (..),
    boundedEnumFrom,
    boundedEnumFromThen,
    fromEnumError,
    predError,
    succError,
    toEnumError,
  )
where

import Data.Bool (Bool (..))
import GHC.Classes (Enum (..))
import GHC.Int (Int (..))
import GHC.Internal.Char (Char)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.Integer (Integer (..), integerToInt#)
import GHC.Num (Num (..))
import GHC.Prim (int2Word#, not#, uncheckedShiftRL#, word2Int#, (+#))

class Bounded a where
  minBound :: a
  maxBound :: a

boundedEnumFrom :: (Enum a, Bounded a) => a -> [a]
boundedEnumFrom value = enumFromTo value maxBound

boundedEnumFromThen :: (Enum a, Bounded a) => a -> a -> [a]
boundedEnumFromThen first second =
  case fromEnum second >= fromEnum first of
    True -> enumFromThenTo first second maxBound
    False -> enumFromThenTo first second minBound

toEnumError :: [Char] -> Int -> (a, a) -> b
toEnumError = toEnumError

fromEnumError :: [Char] -> a -> b
fromEnumError = fromEnumError

succError :: [Char] -> a
succError = succError

predError :: [Char] -> a
predError = predError

instance Bounded Bool where
  minBound = False
  maxBound = True

instance Enum Bool where
  succ False = True
  succ True = succError "Prelude.Enum.Bool.succ"

  pred True = False
  pred False = predError "Prelude.Enum.Bool.pred"

  toEnum (I# value) =
    case value of
      0# -> False
      1# -> True
      _ -> toEnumError "Bool" (I# value) (False, True)

  fromEnum False = I# 0#
  fromEnum True = I# 1#

  enumFrom False = [False, True]
  enumFrom True = [True]

  enumFromThen False True = [False, True]
  enumFromThen True False = [True, False]
  enumFromThen False False = [False]
  enumFromThen True True = [True]

  enumFromTo False False = [False]
  enumFromTo False True = [False, True]
  enumFromTo True True = [True]
  enumFromTo True False = []

  enumFromThenTo False True False = [False]
  enumFromThenTo False True True = [False, True]
  enumFromThenTo True False True = [True]
  enumFromThenTo True False False = [True, False]
  enumFromThenTo False False _ = [False]
  enumFromThenTo True True _ = [True]

instance Bounded Int where
  minBound =
    case maximumInt of
      I# value -> I# ((+#) value 1#)
  maxBound = maximumInt

instance Enum Int where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Int.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Int.pred"
      False -> value - 1

  toEnum value = value
  fromEnum value = value

  enumFrom value = enumFromTo value maxBound

  enumFromThen first second =
    case second >= first of
      True -> enumFromThenTo first second maxBound
      False -> enumFromThenTo first second minBound

  enumFromTo = enumIntFromTo
  enumFromThenTo = enumIntFromThenTo

instance Enum Integer where
  succ value = value + 1
  pred value = value - 1
  toEnum (I# value) = IS value
  fromEnum value = I# (integerToInt# value)
  enumFrom value = enumIntegerFromThen value (value + 1)
  enumFromThen = enumIntegerFromThen
  enumFromTo first = enumIntegerFromThenTo first (first + 1)
  enumFromThenTo = enumIntegerFromThenTo

maximumInt :: Int
maximumInt = I# (word2Int# (uncheckedShiftRL# (not# (int2Word# 0#)) 1#))

enumIntFromTo :: Int -> Int -> [Int]
enumIntFromTo value last =
  case value <= last of
    False -> []
    True ->
      value
        : case value == last of
          True -> []
          False -> enumIntFromTo (value + 1) last

enumIntFromThenTo :: Int -> Int -> Int -> [Int]
enumIntFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            False -> []
            True ->
              value
                : case value + step of
                  next ->
                    case next < value of
                      True -> []
                      False -> go next
        False ->
          case value >= last of
            False -> []
            True ->
              value
                : case value + step of
                  next ->
                    case next > value of
                      True -> []
                      False -> go next

enumIntegerFromThen :: Integer -> Integer -> [Integer]
enumIntegerFromThen first second = first : enumIntegerFromThen second (second + (second - first))

enumIntegerFromThenTo :: Integer -> Integer -> Integer -> [Integer]
enumIntegerFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            True -> value : go (value + step)
            False -> []
        False ->
          case value >= last of
            True -> value : go (value + step)
            False -> []
