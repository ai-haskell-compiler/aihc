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
import GHC.Int (Int)

class Bounded a where
  minBound :: a
  maxBound :: a

class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

boundedEnumFrom :: (Enum a, Bounded a) => a -> [a]
boundedEnumFrom = boundedEnumFrom

boundedEnumFromThen :: (Enum a, Bounded a) => a -> a -> [a]
boundedEnumFromThen = boundedEnumFromThen

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

  toEnum 0 = False
  toEnum 1 = True
  toEnum n = toEnumError "Bool" n (False, True)

  fromEnum False = 0
  fromEnum True = 1

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
