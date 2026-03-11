module Attoparsec.Time.Validation where

import Attoparsec.Time.Prelude

data Validator a
  = Validator String (a -> Bool)

run :: (Show a) => Validator a -> b -> (String -> b) -> a -> b
run (Validator name predicate) onNoError onError input =
  if predicate input
    then onNoError
    else
      onError
        $ showString "Validator "
        $ shows name
        $ showString " failed on the following input: "
        $ show input

month :: (Num a, Ord a) => Validator a
month =
  Validator "month" (liftA2 (&&) (>= 1) (<= 12))

monthDay :: (Num a, Ord a) => Validator a
monthDay =
  Validator "monthDay" (liftA2 (&&) (>= 1) (<= 31))

weekDay :: (Num a, Ord a) => Validator a
weekDay =
  Validator "weekDay" (liftA2 (&&) (>= 1) (<= 7))

hour :: (Num a, Ord a) => Validator a
hour =
  Validator "hour" (liftA2 (&&) (>= 0) (< 24))

minute :: (Num a, Ord a) => Validator a
minute =
  Validator "minute" (liftA2 (&&) (>= 0) (< 60))

second :: (Num a, Ord a) => Validator a
second =
  Validator "second" (liftA2 (&&) (>= 0) (< 60))
