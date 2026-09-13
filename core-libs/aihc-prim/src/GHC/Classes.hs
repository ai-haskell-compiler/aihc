{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use ==" #-}
{-# HLINT ignore "Use /=" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use min" #-}

module GHC.Classes
  ( Eq (..),
    Ord (..),
    (&&),
    (||),
    not,
  )
where

import GHC.Types (Bool (..), Ordering (..))

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  left == right = not (left /= right)
  left /= right = not (left == right)

infix 4 ==, /=

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  compare left right =
    if left == right
      then EQ
      else
        if left <= right
          then LT
          else GT
  left < right =
    case compare left right of
      LT -> True
      _ -> False
  left <= right =
    case compare left right of
      GT -> False
      _ -> True
  left > right =
    case compare left right of
      GT -> True
      _ -> False
  left >= right =
    case compare left right of
      LT -> False
      _ -> True
  max left right = if left <= right then right else left
  min left right = if left <= right then left else right

infix 4 <, <=, >, >=

infixr 3 &&

-- | Boolean conjunction, lazy in its second argument.
(&&) :: Bool -> Bool -> Bool
False && _ = False
True && right = right

infixr 2 ||

-- | Boolean disjunction, lazy in its second argument.
(||) :: Bool -> Bool -> Bool
False || right = right
True || _ = True

-- | Boolean negation.
not :: Bool -> Bool
not False = True
not True = False
