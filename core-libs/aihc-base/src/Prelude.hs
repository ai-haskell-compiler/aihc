{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

module Prelude
  ( Applicative (..),
    Bool (..),
    Char,
    Either (..),
    Eq (..),
    Functor (..),
    Int,
    Integer,
    List (..),
    Maybe (..),
    Monad (..),
    Num (..),
    String,
    (&&),
    (++),
    (/=),
    (==),
    id,
    not,
    otherwise,
    (||),
  )
where

import Data.Bool (Bool (..), not, otherwise, (&&), (||))
import Data.Kind (Type)
import GHC.Int (Int (..))
import GHC.Integer (Integer)
import GHC.Num (Num (..))

foreign import prim (==#) :: Int# -> Int# -> Int#

data Char

data List a = [] | a : [a]

infixr 5 :

type String = [Char]

id :: a -> a
id x = x

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

infix 4 ==, /=

instance Eq Bool where
  False == False = True
  False == True = False
  True == False = False
  True == True = True

  x /= y = not (x == y)

instance Eq Int where
  I# x == I# y =
    case (==#) x y of
      0# -> False
      _ -> True

  x /= y = not (x == y)

instance (Eq a) => Eq [a] where
  [] == [] = True
  [] == (_ : _) = False
  (_ : _) == [] = False
  (x : xs) == (y : ys) = x == y && xs == ys

  xs /= ys = not (xs == ys)

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : (xs ++ ys)

class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap = fmapList

instance Functor Maybe where
  fmap f mx =
    case mx of
      Nothing -> Nothing
      Just x -> Just (f x)

instance Functor (Either e) where
  fmap f mx =
    case mx of
      Left e -> Left e
      Right x -> Right (f x)

class (Functor f) => Applicative (f :: Type -> Type) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

instance Applicative List where
  pure x = [x]

  fs <*> xs = applyList fs xs

instance Applicative Maybe where
  pure = Just

  mf <*> mx =
    case mf of
      Nothing -> Nothing
      Just f ->
        case mx of
          Nothing -> Nothing
          Just x -> Just (f x)

instance Applicative (Either e) where
  pure = Right

  mf <*> mx =
    case mf of
      Left e -> Left e
      Right f ->
        case mx of
          Left e -> Left e
          Right x -> Right (f x)

class (Applicative m) => Monad (m :: Type -> Type) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

infixl 1 >>=, >>

instance Monad List where
  xs >>= k = bindList xs k

  xs >> ys = thenList xs ys
  return x = [x]

instance Monad Maybe where
  mx >>= k = bindMaybe mx k

  mx >> my =
    case mx of
      Nothing -> Nothing
      Just _ -> my
  return = Just

instance Monad (Either e) where
  mx >>= k =
    case mx of
      Left e -> Left e
      Right x -> k x

  mx >> my =
    case mx of
      Left e -> Left e
      Right _ -> my
  return = Right

fmapList :: (a -> b) -> [a] -> [b]
fmapList _ [] = []
fmapList f (x : xs) = f x : fmapList f xs

applyList :: [a -> b] -> [a] -> [b]
applyList [] _ = []
applyList (f : fs) xs = fmapList f xs ++ applyList fs xs

bindList :: [a] -> (a -> [b]) -> [b]
bindList [] _ = []
bindList (x : xs) k = k x ++ bindList xs k

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) k = k x

thenList :: [a] -> [b] -> [b]
thenList [] _ = []
thenList (_ : xs) ys = ys ++ thenList xs ys
