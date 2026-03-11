{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Int.Widen
  ( module X
  , Widen(..)
  ) where

import Data.Int
import Data.Word

import HaskellWorks.Data.Int.Widen.Widen16 as X
import HaskellWorks.Data.Int.Widen.Widen32 as X
import HaskellWorks.Data.Int.Widen.Widen64 as X
import HaskellWorks.Data.Int.Widen.Widen8  as X

class Widen a b where
  -- | Cast the value to a larger size.
  --
  -- This is safer to use than 'Prelude.fromIntegral'.
  widen :: a -> b

instance Widen Int8 Int8 where
  widen = id
  {-# INLINE widen #-}

instance Widen Int8 Int16 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Int8 Int32 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Int8 Int64 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Int16 Int16 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Int16 Int32 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Int16 Int64 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Int32 Int32 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Int32 Int64 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Int64 Int64 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Word8 Word8 where
  widen = id
  {-# INLINE widen #-}

instance Widen Word8 Word16 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Word8 Word32 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Word8 Word64 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Word16 Word16 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Word16 Word32 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Word16 Word64 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Word32 Word32 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Word32 Word64 where
  widen = fromIntegral
  {-# INLINE widen #-}

instance Widen Word64 Word64 where
  widen = fromIntegral
  {-# INLINE widen #-}
