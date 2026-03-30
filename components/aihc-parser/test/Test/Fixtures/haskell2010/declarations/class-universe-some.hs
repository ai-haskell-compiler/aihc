{- ORACLE_TEST
id: decls-class-universe-some
category: declarations
expected: pass
reason: parser supports universe-some style class declarations
-}
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
