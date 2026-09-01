module GHC.Classes
  ( Eq (..),
    Enum (..),
    Ord (..),
  )
where

import GHC.Types (Bool, Int, Ordering)

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

infix 4 ==, /=

class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

infix 4 <, <=, >, >=
