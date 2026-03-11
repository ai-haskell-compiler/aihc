{-# LANGUAGE DeriveGeneric, FlexibleContexts, UndecidableInstances #-}

module Data.Lexicographic.Generic
  (module Data.Lexicographic.Generic, GBounded, GEnum, Generic, Rep)
where

import Data.Lexicographic.Generic.Internal
import GHC.Generics

-- | Derive generic instances of 'Bounded' and 'Enum' for sum types, and 'Enum'
-- instances for product types, based on the instances of the components.
--
-- These instances are designed to cohere with the stock-derived 'Ord' instance.
--
-- > data A = A1 Bool | A2 Ordering
-- >   deriving (Generic, Ord)
-- >   deriving (Bounded, Enum) via GenericLexicographicOrder A
--
-- >>> [minBound .. maxBound] :: [A]
-- [A1 False, A1 True, A2 LT, A2 EQ, A2 GT]
--
-- >>> succ (A1 maxBound)
-- A2 LT
--
-- >>> pred (A2 minBound)
-- A1 True
--
-- >>> A1 maxBound < A2 minBound
-- True
newtype GenericLexicographicOrder a = GenericLexicographicOrder a
  deriving (Eq, Ord, Read, Show, Generic)

instance (Generic a, GBounded (Rep a)) => Bounded (GenericLexicographicOrder a) where
  minBound = GenericLexicographicOrder (to gminBound)
  maxBound = GenericLexicographicOrder (to gmaxBound)

instance (Generic a, GEnum (Rep a)) => Enum (GenericLexicographicOrder a) where
  fromEnum (GenericLexicographicOrder x) = gfromEnum (from x)
  toEnum n = GenericLexicographicOrder (to (gtoEnum n))

genericMinBound :: (Generic a, GBounded (Rep a)) => a
genericMinBound = case minBound of GenericLexicographicOrder x -> x

genericMaxBound :: (Generic a, GBounded (Rep a)) => a
genericMaxBound = case maxBound of GenericLexicographicOrder x -> x

genericFromEnum :: (Generic a, GEnum (Rep a)) => a -> Int
genericFromEnum x = fromEnum $ GenericLexicographicOrder x

genericToEnum :: (Generic a, GEnum (Rep a)) => Int -> a
genericToEnum n = case toEnum n of GenericLexicographicOrder x -> x
