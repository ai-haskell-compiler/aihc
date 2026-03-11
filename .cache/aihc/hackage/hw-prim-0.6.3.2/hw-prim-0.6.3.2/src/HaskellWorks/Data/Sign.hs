{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Sign
    ( Sign(..)
    ) where

import Data.Int
import Data.Word

class Sign a where
  type SignOf a
  sign :: a -> SignOf a

instance Sign Word where
  type SignOf Word = Int
  sign = fromIntegral
  {-# INLINE sign #-}

instance Sign Word8 where
  type SignOf Word8 = Int8
  sign = fromIntegral
  {-# INLINE sign #-}

instance Sign Word16 where
  type SignOf Word16 = Int16
  sign = fromIntegral
  {-# INLINE sign #-}

instance Sign Word32 where
  type SignOf Word32 = Int32
  sign = fromIntegral
  {-# INLINE sign #-}

instance Sign Word64 where
  type SignOf Word64 = Int64
  sign = fromIntegral
  {-# INLINE sign #-}

instance Sign Int where
  type SignOf Int = Int
  sign = fromIntegral
  {-# INLINE sign #-}

instance Sign Int8 where
  type SignOf Int8 = Int8
  sign = fromIntegral
  {-# INLINE sign #-}

instance Sign Int16 where
  type SignOf Int16 = Int16
  sign = fromIntegral
  {-# INLINE sign #-}

instance Sign Int32 where
  type SignOf Int32 = Int32
  sign = fromIntegral
  {-# INLINE sign #-}

instance Sign Int64 where
  type SignOf Int64 = Int64
  sign = fromIntegral
  {-# INLINE sign #-}
