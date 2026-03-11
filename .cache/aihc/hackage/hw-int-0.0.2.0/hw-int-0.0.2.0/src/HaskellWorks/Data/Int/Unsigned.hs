{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Unsigned
  ( Unsigned(..)
  ) where

import Data.Int
import Data.Word

class Unsigned a where
  type UnsignedOf a

  -- | Cast the value to a larger size.
  --
  -- This is safer to use than 'Prelude.fromIntegral'.
  unsigned :: a -> UnsignedOf a

instance Unsigned Int8 where
  type UnsignedOf Int8 = Word8

  unsigned = fromIntegral
  {-# INLINE unsigned #-}

instance Unsigned Int16 where
  type UnsignedOf Int16 = Word16

  unsigned = fromIntegral
  {-# INLINE unsigned #-}

instance Unsigned Int32 where
  type UnsignedOf Int32 = Word32

  unsigned = fromIntegral
  {-# INLINE unsigned #-}

instance Unsigned Int64 where
  type UnsignedOf Int64 = Word64

  unsigned = fromIntegral
  {-# INLINE unsigned #-}
