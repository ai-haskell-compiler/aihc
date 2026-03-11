{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Widen.Widen16
  ( Widen16(..)
  ) where

import Data.Int
import Data.Word

class Widen16 a where
  type Widened16 a
  widen16 :: a -> Widened16 a

instance Widen16 Word8 where
  type Widened16 Word8 = Word16
  widen16 = fromIntegral
  {-# INLINE widen16 #-}

instance Widen16 Word16 where
  type Widened16 Word16 = Word16
  widen16 = fromIntegral
  {-# INLINE widen16 #-}

instance Widen16 Int8 where
  type Widened16 Int8 = Int16
  widen16 = fromIntegral
  {-# INLINE widen16 #-}

instance Widen16 Int16 where
  type Widened16 Int16 = Int16
  widen16 = fromIntegral
  {-# INLINE widen16 #-}
