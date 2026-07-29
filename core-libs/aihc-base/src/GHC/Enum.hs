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

import GHC.Int (Int)
import Prelude (Bool (..), Bounded (..), Char, Enum (..), Ord (..))

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
