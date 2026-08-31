module GHC.Classes
  ( Eq (..),
    Ord (..),
  )
where

import GHC.Types (Bool (..), Ordering (..))

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  left /= right =
    case left == right of
      False -> True
      True -> False

infix 4 ==, /=

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  left < right =
    case compare left right of
      LT -> True
      _ -> False
  (<=) :: a -> a -> Bool
  left <= right =
    case compare left right of
      GT -> False
      _ -> True
  (>) :: a -> a -> Bool
  left > right =
    case compare left right of
      GT -> True
      _ -> False
  (>=) :: a -> a -> Bool
  left >= right =
    case compare left right of
      LT -> False
      _ -> True
  max :: a -> a -> a
  max left right =
    case left <= right of
      True -> right
      False -> left
  min :: a -> a -> a
  min left right =
    case left <= right of
      True -> left
      False -> right

infix 4 <, <=, >, >=
