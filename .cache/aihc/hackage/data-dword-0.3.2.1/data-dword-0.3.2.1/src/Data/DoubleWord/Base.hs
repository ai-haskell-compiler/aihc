{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DoubleWord.Base
  ( DoubleWord(..)
  ) where

import Data.Bits (Bits(..))
import Data.Int
import Data.Word
import Data.BinaryWord

-- | Defines a particular way to split a binary word in halves.
class BinaryWord w ⇒ DoubleWord w where
  -- | The low half type
  type LoWord w
  -- | The high half type
  type HiWord w
  -- | The low half of the word
  loWord      ∷ w → LoWord w
  -- | The high half of the word
  hiWord      ∷ w → HiWord w
  -- | Construct a word from the low and high halves
  fromHiAndLo ∷ HiWord w → LoWord w → w
  -- | Extend the low half
  extendLo ∷ LoWord w → w
  -- | Sign-extend the low half
  signExtendLo ∷ SignedWord (LoWord w) → w

instance DoubleWord Word16 where
  type LoWord Word16 = Word8
  type HiWord Word16 = Word8
  loWord w = fromIntegral w
  {-# INLINE loWord #-}
  hiWord w = fromIntegral $ shiftR w 8
  {-# INLINE hiWord #-}
  fromHiAndLo hi lo = shiftL (fromIntegral hi) 8 .|. fromIntegral lo
  {-# INLINE fromHiAndLo #-}
  extendLo = fromIntegral
  {-# INLINE extendLo #-}
  signExtendLo = fromIntegral
  {-# INLINE signExtendLo #-}

instance DoubleWord Word32 where
  type LoWord Word32 = Word16
  type HiWord Word32 = Word16
  loWord w = fromIntegral w
  {-# INLINE loWord #-}
  hiWord w = fromIntegral $ shiftR w 16
  {-# INLINE hiWord #-}
  fromHiAndLo hi lo = shiftL (fromIntegral hi) 16 .|. fromIntegral lo
  {-# INLINE fromHiAndLo #-}
  extendLo = fromIntegral
  {-# INLINE extendLo #-}
  signExtendLo = fromIntegral
  {-# INLINE signExtendLo #-}

instance DoubleWord Word64 where
  type LoWord Word64 = Word32
  type HiWord Word64 = Word32
  loWord w = fromIntegral w
  {-# INLINE loWord #-}
  hiWord w = fromIntegral $ shiftR w 32
  {-# INLINE hiWord #-}
  fromHiAndLo hi lo = shiftL (fromIntegral hi) 32 .|. fromIntegral lo
  {-# INLINE fromHiAndLo #-}
  extendLo = fromIntegral
  {-# INLINE extendLo #-}
  signExtendLo = fromIntegral
  {-# INLINE signExtendLo #-}

instance DoubleWord Int16 where
  type LoWord Int16 = Word8
  type HiWord Int16 = Int8
  loWord w = fromIntegral w
  {-# INLINE loWord #-}
  hiWord w = fromIntegral $ shiftR w 8
  {-# INLINE hiWord #-}
  fromHiAndLo hi lo = shiftL (fromIntegral hi) 8 .|. fromIntegral lo
  {-# INLINE fromHiAndLo #-}
  extendLo = fromIntegral
  {-# INLINE extendLo #-}
  signExtendLo = fromIntegral
  {-# INLINE signExtendLo #-}

instance DoubleWord Int32 where
  type LoWord Int32 = Word16
  type HiWord Int32 = Int16
  loWord w = fromIntegral w
  {-# INLINE loWord #-}
  hiWord w = fromIntegral $ shiftR w 16
  {-# INLINE hiWord #-}
  fromHiAndLo hi lo = shiftL (fromIntegral hi) 16 .|. fromIntegral lo
  {-# INLINE fromHiAndLo #-}
  extendLo = fromIntegral
  {-# INLINE extendLo #-}
  signExtendLo = fromIntegral
  {-# INLINE signExtendLo #-}

instance DoubleWord Int64 where
  type LoWord Int64 = Word32
  type HiWord Int64 = Int32
  loWord w = fromIntegral w
  {-# INLINE loWord #-}
  hiWord w = fromIntegral $ shiftR w 32
  {-# INLINE hiWord #-}
  fromHiAndLo hi lo = shiftL (fromIntegral hi) 32 .|. fromIntegral lo
  {-# INLINE fromHiAndLo #-}
  extendLo = fromIntegral
  {-# INLINE extendLo #-}
  signExtendLo = fromIntegral
  {-# INLINE signExtendLo #-}

