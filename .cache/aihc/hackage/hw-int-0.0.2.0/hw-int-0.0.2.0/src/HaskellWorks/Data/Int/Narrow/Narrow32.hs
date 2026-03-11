{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Narrow.Narrow32
  ( Narrow32(..)
  ) where

import Data.Int
import Data.Word

class Narrow32 a where
  type Narrowed32 a
  narrow32 :: a -> Narrowed32 a

instance Narrow32 Word32 where
  type Narrowed32 Word32 = Word32
  narrow32 = fromIntegral
  {-# INLINE narrow32 #-}

instance Narrow32 Word64 where
  type Narrowed32 Word64 = Word32
  narrow32 = fromIntegral
  {-# INLINE narrow32 #-}

instance Narrow32 Int32 where
  type Narrowed32 Int32 = Int32
  narrow32 = fromIntegral
  {-# INLINE narrow32 #-}

instance Narrow32 Int64 where
  type Narrowed32 Int64 = Int32
  narrow32 = fromIntegral
  {-# INLINE narrow32 #-}
