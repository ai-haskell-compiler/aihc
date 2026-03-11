{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Widen.Widen8
  ( Widen8(..)
  ) where

import Data.Int
import Data.Word

class Widen8 a where
  type Widened8 a
  widen8 :: a -> Widened8 a

instance Widen8 Word8 where
  type Widened8 Word8 = Word8
  widen8 = fromIntegral
  {-# INLINE widen8 #-}

instance Widen8 Int8 where
  type Widened8 Int8 = Int8
  widen8 = fromIntegral
  {-# INLINE widen8 #-}
