{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Int.Signed
  ( Signed(..)
  ) where

import Data.Int
import Data.Word

class Signed a where
  type SignedOf a

  -- | Cast the value to a larger size.
  --
  -- This is safer to use than 'Prelude.fromIntegral'.
  signed :: a -> SignedOf a

instance Signed Word8 where
  type SignedOf Word8 = Int8

  signed = fromIntegral
  {-# INLINE signed #-}

instance Signed Word16 where
  type SignedOf Word16 = Int16

  signed = fromIntegral
  {-# INLINE signed #-}

instance Signed Word32 where
  type SignedOf Word32 = Int32

  signed = fromIntegral
  {-# INLINE signed #-}

instance Signed Word64 where
  type SignedOf Word64 = Int64

  signed = fromIntegral
  {-# INLINE signed #-}
