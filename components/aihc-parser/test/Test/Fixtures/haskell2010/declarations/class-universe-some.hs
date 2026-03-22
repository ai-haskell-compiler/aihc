{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module UniverseSome (
  UniverseSome (..),
  FiniteSome (..),
  ) where

import Data.List (genericLength)

class UniverseSome f where
  universeSome :: [f a]

class UniverseSome f => FiniteSome f where
  universeFSome :: [f a]
  universeFSome = universeSome

  cardinalitySome :: Int
  cardinalitySome = genericLength (universeFSome :: [f Int])
