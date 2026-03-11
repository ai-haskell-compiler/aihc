{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Narrow.Narrow16
  ( Narrow16(..)
  ) where

import Data.Int
import Data.Word

class Narrow16 a where
  type Narrowed16 a
  narrow16 :: a -> Narrowed16 a

instance Narrow16 Word16 where
  type Narrowed16 Word16 = Word16
  narrow16 = fromIntegral
  {-# INLINE narrow16 #-}

instance Narrow16 Word32 where
  type Narrowed16 Word32 = Word16
  narrow16 = fromIntegral
  {-# INLINE narrow16 #-}

instance Narrow16 Word64 where
  type Narrowed16 Word64 = Word16
  narrow16 = fromIntegral
  {-# INLINE narrow16 #-}

instance Narrow16 Int16 where
  type Narrowed16 Int16 = Int16
  narrow16 = fromIntegral
  {-# INLINE narrow16 #-}

instance Narrow16 Int32 where
  type Narrowed16 Int32 = Int16
  narrow16 = fromIntegral
  {-# INLINE narrow16 #-}

instance Narrow16 Int64 where
  type Narrowed16 Int64 = Int16
  narrow16 = fromIntegral
  {-# INLINE narrow16 #-}
