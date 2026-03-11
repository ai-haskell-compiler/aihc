-- |
-- Module       : Data.ByteString.Base16.Types.Internal
-- Copyright    : (c) 2019-2022 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                sofia-m-a <https://github.com/sofia-m-a>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains the 'Base16' newtype.
--
module Data.Base16.Types.Internal
( Base16(..)
) where


import Control.DeepSeq

import Data.Functor.Classes

-- | Wraps a value, asserting that it is or is intended to be
-- in a particular kind of Base16 encoding use 'extractBase16'
-- to extract the value, and 'assertBase16' to tag a value
-- as base16-encoded
--
newtype Base16 a = Base16 a

instance Eq a => Eq (Base16 a) where
  Base16 a == Base16 b = a == b

instance Eq1 Base16 where
  liftEq f (Base16 a) (Base16 b) = f a b

instance Ord a => Ord (Base16 a) where
  compare (Base16 a) (Base16 b) = compare a b

instance Ord1 Base16 where
  liftCompare f (Base16 a) (Base16 b) = f a b

instance Functor Base16 where
  fmap f (Base16 a) = Base16 (f a)

instance Applicative Base16 where
  pure = Base16
  Base16 f <*> Base16 a = Base16 (f a)

instance Monad Base16 where
  return = pure
  Base16 a >>= k = k a

instance Show a => Show (Base16 a) where
  show (Base16 a) = show a

instance NFData a => NFData (Base16 a) where
  rnf (Base16 a) = rnf a
