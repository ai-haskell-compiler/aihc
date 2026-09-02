{-# LANGUAGE TypeFamilies #-}

module GHC.IsList
  ( IsList (..),
    Item,
  )
where

import Data.Kind (Type)
import Prelude (Int)

-- | The element type of a list-like container.
-- Associated type declarations are not available, so the family is top level.
type family Item l :: Type

class IsList l where
  fromList :: [Item l] -> l
  fromListN :: Int -> [Item l] -> l
  toList :: l -> [Item l]

  fromListN _ = fromList
