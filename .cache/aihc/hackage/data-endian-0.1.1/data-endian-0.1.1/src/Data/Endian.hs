{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if MIN_VERSION_base(4,6,0)
{-# LANGUAGE DeriveGeneric #-}
#endif

module Data.Endian
  ( Endian(..)
  , isLittleEndian
  , isBigEndian
  , EndianSensitive(..)
  , toLittleEndian
  , fromLittleEndian
  , toBigEndian
  , fromBigEndian
  , toEndian
  , fromEndian
  ) where

import Data.Typeable (Typeable)
import Data.Data (Data)
#if MIN_VERSION_base(4,6,0)
import GHC.Generics (Generic)
#endif
import Data.Ix (Ix)
import Data.Int
import Data.Word
import Data.Bits
import Foreign.C.Types
import Foreign.Ptr (IntPtr, WordPtr)
import System.Posix.Types (CSsize)

#include <HsBaseConfig.h>

-- | Endianness.
data Endian = LittleEndian -- ^ Little-endian
            | BigEndian    -- ^ Big-endian
            deriving (Typeable, Data, Show, Read,
#if MIN_VERSION_base(4,6,0)
                      Generic,
#endif
                      Eq, Ord, Bounded, Enum, Ix)

-- | Return 'True' if the supplied value is 'LittleEndian'.
isLittleEndian ∷ Endian → Bool
isLittleEndian LittleEndian = True
isLittleEndian BigEndian    = False

-- | Return 'True' if the supplied value is 'BigEndian'.
isBigEndian ∷ Endian → Bool
isBigEndian LittleEndian = False
isBigEndian BigEndian    = True

-- | Raw, endian-sensitive data.
class EndianSensitive α where
  -- | Change the endianness of the argument.
  swapEndian ∷ α → α

-- | Convert from the native format to little-endian.
toLittleEndian   ∷ EndianSensitive α ⇒ α → α
-- | Convert from little-endian to the native format.
fromLittleEndian ∷ EndianSensitive α ⇒ α → α
-- | Convert from the native format to big-endian.
toBigEndian      ∷ EndianSensitive α ⇒ α → α
-- | Convert from big-endian to the native format.
fromBigEndian    ∷ EndianSensitive α ⇒ α → α

#ifdef WORDS_BIGENDIAN
toLittleEndian   = swapEndian
toBigEndian      = id
#else
toLittleEndian   = id
toBigEndian      = swapEndian
#endif

fromLittleEndian = toLittleEndian
fromBigEndian    = toBigEndian

{-# INLINE toLittleEndian #-}
{-# INLINE fromLittleEndian #-}
{-# INLINE toBigEndian #-}
{-# INLINE fromBigEndian #-}

-- | Convert from the native format to the specified endianness.
toEndian ∷ EndianSensitive α ⇒ Endian → α → α
toEndian LittleEndian = toLittleEndian
toEndian BigEndian    = toBigEndian

-- | Convert from the specified endianness to the native format.
fromEndian ∷ EndianSensitive α ⇒ Endian → α → α
fromEndian = toEndian
{-# INLINE fromEndian #-}

instance EndianSensitive α ⇒ EndianSensitive [α] where
  swapEndian = map swapEndian

instance EndianSensitive Word16 where
  swapEndian = (`rotateR` 8)
  {-# INLINE swapEndian #-}

instance EndianSensitive Word32 where
  swapEndian x  =  (x                  `shiftR` 24)
               .|. ((x .&. 0x00FF0000) `shiftR` 8)
               .|. ((x .&. 0x0000FF00) `shiftL` 8)
               .|. (x                  `shiftL` 24)
  {-# INLINE swapEndian #-}

instance EndianSensitive Word64 where
  swapEndian x
#if WORD_SIZE_IN_BITS == 32
    =  fromIntegral (swapEndian hi)
   .|. (fromIntegral (swapEndian lo) `shiftL` 32)
      where lo, hi ∷ Word32
            lo = fromIntegral x
            hi = fromIntegral (x `shiftR` 32)
#else
    =  (x                          `shiftR` 56)
   .|. ((x .&. 0x00FF000000000000) `shiftR` 40)
   .|. ((x .&. 0x0000FF0000000000) `shiftR` 24)
   .|. ((x .&. 0x000000FF00000000) `shiftR` 8)
   .|. ((x .&. 0x00000000FF000000) `shiftL` 8)
   .|. ((x .&. 0x0000000000FF0000) `shiftL` 24)
   .|. ((x .&. 0x000000000000FF00) `shiftL` 40)
   .|. (x                          `shiftL` 56)
#endif
  {-# INLINE swapEndian #-}

instance EndianSensitive Int16 where
  swapEndian = (`rotateR` 8)
  {-# INLINE swapEndian #-}

instance EndianSensitive Int32 where
  swapEndian = fromIntegral . (swapEndian ∷ Word32 → Word32) . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive Int64 where
  swapEndian = fromIntegral . (swapEndian ∷ Word64 → Word64) . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CShort where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_SHORT → HTYPE_SHORT)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CUShort where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_UNSIGNED_SHORT → HTYPE_UNSIGNED_SHORT)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CInt where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_INT → HTYPE_INT)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CUInt where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_UNSIGNED_INT → HTYPE_UNSIGNED_INT)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CLong where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_LONG → HTYPE_LONG)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CULong where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_UNSIGNED_LONG → HTYPE_UNSIGNED_LONG)
             . fromIntegral
  {-# INLINE swapEndian #-}

#ifdef HAVE_LONG_LONG
instance EndianSensitive CLLong where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_LONG_LONG → HTYPE_LONG_LONG)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CULLong where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_UNSIGNED_LONG_LONG
                           → HTYPE_UNSIGNED_LONG_LONG)
             . fromIntegral
  {-# INLINE swapEndian #-}
#endif

instance EndianSensitive CSize where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_SIZE_T → HTYPE_SIZE_T)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CSsize where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_SSIZE_T → HTYPE_SSIZE_T)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CUIntPtr where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_UINTPTR_T → HTYPE_UINTPTR_T)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CIntPtr where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_INTPTR_T → HTYPE_INTPTR_T)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CPtrdiff where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_PTRDIFF_T → HTYPE_PTRDIFF_T)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CUIntMax where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_UINTMAX_T → HTYPE_UINTMAX_T)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive CIntMax where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_INTMAX_T → HTYPE_INTMAX_T)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive WordPtr where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_UINTPTR_T → HTYPE_UINTPTR_T)
             . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive IntPtr where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_INTPTR_T → HTYPE_INTPTR_T)
             . fromIntegral
  {-# INLINE swapEndian #-}

#ifdef HTYPE_WCHAR_T
instance EndianSensitive CWchar where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_WCHAR_T → HTYPE_WCHAR_T)
             . fromIntegral
  {-# INLINE swapEndian #-}
#endif

#ifdef HTYPE_WINT_T
instance EndianSensitive CWint where
  swapEndian = fromIntegral
             . (swapEndian ∷ HTYPE_WINT_T → HTYPE_WINT_T)
             . fromIntegral
  {-# INLINE swapEndian #-}
#endif
