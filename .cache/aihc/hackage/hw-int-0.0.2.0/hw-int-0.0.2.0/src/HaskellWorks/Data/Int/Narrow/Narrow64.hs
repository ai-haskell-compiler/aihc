{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Narrow.Narrow64
  ( Narrow64(..)
  ) where

import Data.Int
import Data.Word

class Narrow64 a where
  type Narrowed64 a
  narrow64 :: a -> Narrowed64 a

instance Narrow64 Word64 where
  type Narrowed64 Word64 = Word64
  narrow64 = fromIntegral
  {-# INLINE narrow64 #-}

instance Narrow64 Int64 where
  type Narrowed64 Int64 = Int64
  narrow64 = fromIntegral
  {-# INLINE narrow64 #-}
