{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Int.Narrow
  ( module X
  , Narrow(..)
  ) where

import Data.Int
import Data.Word

import HaskellWorks.Data.Int.Narrow.Narrow16 as X
import HaskellWorks.Data.Int.Narrow.Narrow32 as X
import HaskellWorks.Data.Int.Narrow.Narrow64 as X
import HaskellWorks.Data.Int.Narrow.Narrow8  as X

class Narrow a b where
  -- | Cast the value to a smaller size.
  --
  -- If the value does not fit in the smaller size, data will be lost.
  --
  -- This gives a better indication of intent than `Prelude.fromIntegral'.
  narrow :: a -> b

instance Narrow Int8 Int8 where
  narrow = id
  {-# INLINE narrow #-}

instance Narrow Int16 Int8 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Int32 Int8 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Int64 Int8 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Int16 Int16 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Int32 Int16 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Int64 Int16 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Int32 Int32 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Int64 Int32 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Int64 Int64 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Word8 Word8 where
  narrow = id
  {-# INLINE narrow #-}

instance Narrow Word16 Word8 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Word32 Word8 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Word64 Word8 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Word16 Word16 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Word32 Word16 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Word64 Word16 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Word32 Word32 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Word64 Word32 where
  narrow = fromIntegral
  {-# INLINE narrow #-}

instance Narrow Word64 Word64 where
  narrow = fromIntegral
  {-# INLINE narrow #-}
