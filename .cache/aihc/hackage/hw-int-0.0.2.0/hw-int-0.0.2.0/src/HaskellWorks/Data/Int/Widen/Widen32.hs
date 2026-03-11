{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Widen.Widen32
  ( Widen32(..)
  ) where

import Data.Int
import Data.Word

class Widen32 a where
  type Widened32 a
  widen32 :: a -> Widened32 a

instance Widen32 Word8 where
  type Widened32 Word8 = Word32
  widen32 = fromIntegral
  {-# INLINE widen32 #-}

instance Widen32 Word16 where
  type Widened32 Word16 = Word32
  widen32 = fromIntegral
  {-# INLINE widen32 #-}

instance Widen32 Word32 where
  type Widened32 Word32 = Word32
  widen32 = fromIntegral
  {-# INLINE widen32 #-}

instance Widen32 Int8 where
  type Widened32 Int8 = Int32
  widen32 = fromIntegral
  {-# INLINE widen32 #-}

instance Widen32 Int16 where
  type Widened32 Int16 = Int32
  widen32 = fromIntegral
  {-# INLINE widen32 #-}

instance Widen32 Int32 where
  type Widened32 Int32 = Int32
  widen32 = fromIntegral
  {-# INLINE widen32 #-}
