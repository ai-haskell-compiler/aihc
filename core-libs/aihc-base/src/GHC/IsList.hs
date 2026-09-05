{-# LANGUAGE TypeFamilies #-}

module GHC.IsList
  ( IsList (..),
  )
where

import Data.Kind (Type)
import Prelude (Int)

-- | Structures that list literals can build and take apart.
class IsList l where
  -- | The element type of the structure.
  type Item l :: Type

  fromList :: [Item l] -> l
  fromListN :: Int -> [Item l] -> l
  toList :: l -> [Item l]

  fromListN _ = fromList
