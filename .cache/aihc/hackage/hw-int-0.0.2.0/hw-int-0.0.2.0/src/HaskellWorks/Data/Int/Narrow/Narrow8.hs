{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Narrow.Narrow8
  ( Narrow8(..)
  ) where

import Data.Int
import Data.Word

class Narrow8 a where
  type Narrowed8 a
  narrow8 :: a -> Narrowed8 a

instance Narrow8 Word8 where
  type Narrowed8 Word8 = Word8
  narrow8 = fromIntegral
  {-# INLINE narrow8 #-}

instance Narrow8 Word16 where
  type Narrowed8 Word16 = Word8
  narrow8 = fromIntegral
  {-# INLINE narrow8 #-}

instance Narrow8 Word32 where
  type Narrowed8 Word32 = Word8
  narrow8 = fromIntegral
  {-# INLINE narrow8 #-}

instance Narrow8 Word64 where
  type Narrowed8 Word64 = Word8
  narrow8 = fromIntegral
  {-# INLINE narrow8 #-}

instance Narrow8 Int8 where
  type Narrowed8 Int8 = Int8
  narrow8 = fromIntegral
  {-# INLINE narrow8 #-}

instance Narrow8 Int16 where
  type Narrowed8 Int16 = Int8
  narrow8 = fromIntegral
  {-# INLINE narrow8 #-}

instance Narrow8 Int32 where
  type Narrowed8 Int32 = Int8
  narrow8 = fromIntegral
  {-# INLINE narrow8 #-}

instance Narrow8 Int64 where
  type Narrowed8 Int64 = Int8
  narrow8 = fromIntegral
  {-# INLINE narrow8 #-}
