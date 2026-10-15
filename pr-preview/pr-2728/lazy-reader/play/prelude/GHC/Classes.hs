{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use /=" #-}

-- | Equality and order for the small Prelude of The Lazy Reader.
--
-- The guide compiles the modules of a reader against this Prelude and the
-- primitive package of aihc. The module names follow the core libraries,
-- so deriving and the literal syntax find the classes where the type
-- checker expects them.
module GHC.Classes
  ( Eq (..),
    Ord (..),
    (&&),
    (||),
    not,
  )
where

import GHC.Prim (eqChar#, gtChar#, ltChar#, (<#), (==#), (>#))
import GHC.Types (Bool (..), Char (..), Int (..), Ordering (..), isTrue#)

infix 4 ==, /=, <, <=, >, >=

infixr 3 &&

infixr 2 ||

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  x < y = case compare x y of
    LT -> True
    _ -> False
  x <= y = case compare x y of
    GT -> False
    _ -> True
  x > y = case compare x y of
    GT -> True
    _ -> False
  x >= y = case compare x y of
    LT -> False
    _ -> True

(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False

(||) :: Bool -> Bool -> Bool
True || _ = True
False || x = x

not :: Bool -> Bool
not True = False
not False = True

instance Eq Int where
  I# x == I# y = isTrue# (x ==# y)

instance Ord Int where
  compare (I# x) (I# y) =
    if isTrue# (x <# y) then LT else if isTrue# (x ># y) then GT else EQ

instance Eq Char where
  C# x == C# y = isTrue# (eqChar# x y)

instance Ord Char where
  compare (C# x) (C# y) =
    if isTrue# (ltChar# x y) then LT else if isTrue# (gtChar# x y) then GT else EQ

instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False

instance Ord Bool where
  compare False True = LT
  compare True False = GT
  compare _ _ = EQ

instance Eq Ordering where
  LT == LT = True
  EQ == EQ = True
  GT == GT = True
  _ == _ = False

instance Eq () where
  () == () = True

instance Ord () where
  compare () () = EQ

instance (Eq a) => Eq [a] where
  [] == [] = True
  (x : xs) == (y : ys) = x == y && xs == ys
  _ == _ = False

instance (Ord a) => Ord [a] where
  compare [] [] = EQ
  compare [] (_ : _) = LT
  compare (_ : _) [] = GT
  compare (x : xs) (y : ys) = case compare x y of
    EQ -> compare xs ys
    other -> other

instance (Eq a, Eq b) => Eq (a, b) where
  (a, b) == (c, d) = a == c && b == d

instance (Ord a, Ord b) => Ord (a, b) where
  compare (a, b) (c, d) = case compare a c of
    EQ -> compare b d
    other -> other
