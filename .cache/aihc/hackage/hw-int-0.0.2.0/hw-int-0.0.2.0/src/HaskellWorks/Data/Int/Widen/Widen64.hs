{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Widen.Widen64
  ( Widen64(..)
  ) where

import Data.Int
import Data.Word

class Widen64 a where
  type Widened64 a
  widen64 :: a -> Widened64 a

instance Widen64 Word8 where
  type Widened64 Word8 = Word64
  widen64 = fromIntegral
  {-# INLINE widen64 #-}

instance Widen64 Word16 where
  type Widened64 Word16 = Word64
  widen64 = fromIntegral
  {-# INLINE widen64 #-}

instance Widen64 Word32 where
  type Widened64 Word32 = Word64
  widen64 = fromIntegral
  {-# INLINE widen64 #-}

instance Widen64 Word64 where
  type Widened64 Word64 = Word64
  widen64 = id
  {-# INLINE widen64 #-}

instance Widen64 Int8 where
  type Widened64 Int8 = Int64
  widen64 = fromIntegral
  {-# INLINE widen64 #-}

instance Widen64 Int16 where
  type Widened64 Int16 = Int64
  widen64 = fromIntegral
  {-# INLINE widen64 #-}

instance Widen64 Int32 where
  type Widened64 Int32 = Int64
  widen64 = fromIntegral
  {-# INLINE widen64 #-}

instance Widen64 Int64 where
  type Widened64 Int64 = Int64
  widen64 = id
  {-# INLINE widen64 #-}
