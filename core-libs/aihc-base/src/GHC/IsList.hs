{-# LANGUAGE TypeFamilies #-}

module GHC.IsList
  ( IsList (..),
  )
where

import Data.Kind (Type)
import GHC.Err (errorWithoutStackTrace)
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import Prelude (Int)

-- | Structures that list literals can build and take apart.
class IsList l where
  -- | The element type of the structure.
  type Item l :: Type

  fromList :: [Item l] -> l
  fromListN :: Int -> [Item l] -> l
  toList :: l -> [Item l]

  fromListN _ = fromList

instance IsList [a] where
  type Item [a] = a
  fromList xs = xs
  fromListN _ xs = xs
  toList xs = xs

instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a
  fromList [] = errorWithoutStackTrace "NonEmpty.fromList: empty list"
  fromList (value : values) = value :| values
  toList (value :| values) = value : values
