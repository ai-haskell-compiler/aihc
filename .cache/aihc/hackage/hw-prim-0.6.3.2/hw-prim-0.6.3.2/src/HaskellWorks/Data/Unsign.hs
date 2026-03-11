{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Unsign
    ( Unsign(..)
    ) where

import Data.Int
import Data.Word

class Unsign a where
  type UnsignOf a
  unsign :: a -> UnsignOf a

instance Unsign Int where
  type UnsignOf Int = Word
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsign Int8 where
  type UnsignOf Int8 = Word8
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsign Int16 where
  type UnsignOf Int16 = Word16
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsign Int32 where
  type UnsignOf Int32 = Word32
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsign Int64 where
  type UnsignOf Int64 = Word64
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsign Word where
  type UnsignOf Word = Word
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsign Word8 where
  type UnsignOf Word8 = Word8
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsign Word16 where
  type UnsignOf Word16 = Word16
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsign Word32 where
  type UnsignOf Word32 = Word32
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsign Word64 where
  type UnsignOf Word64 = Word64
  unsign = fromIntegral
  {-# INLINE unsign #-}
