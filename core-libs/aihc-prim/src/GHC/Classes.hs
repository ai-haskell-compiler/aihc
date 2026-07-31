module GHC.Classes
  ( Eq (..),
    Ord (..),
  )
where

import GHC.Types (Bool, Ordering)

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

infix 4 ==, /=

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

infix 4 <, <=, >, >=
