{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  #-}

module Data.Mergeable where

import Data.Commutative
import Data.List.NonEmpty as NE
import qualified Data.Vector as Vector


class Mergeable t where
  mergeMap :: CommutativeId m => (a -> m) -> t a -> m
  merge :: (a -> b -> b) -> b -> t a -> b

  mergeMap f = merge (commute . f) cempty
  merge f i xs = appCommEndo (mergeMap (CommEndo . f) xs) i

instance Mergeable [] where
  mergeMap _ [] = cempty
  mergeMap f (x:xs) = f x <~> mergeMap f xs

instance Mergeable Vector.Vector where
  mergeMap f xss
    | Vector.null xss = cempty
    | otherwise = f (Vector.head xss) <~> mergeMap f (Vector.drop 1 xss)


class Functor t => Mergeable1 t where
  mergeMap1 :: Commutative m => (a -> m) -> t a -> m
  merge1 :: Commutative m => t m -> m

  mergeMap1 f = merge1 . fmap f
  merge1 = mergeMap1 id

instance Mergeable1 NonEmpty where
  mergeMap1 f (x:|[]) = f x
  mergeMap1 f (x:|xs) = f x <~> mergeMap1 f (NE.fromList xs)
