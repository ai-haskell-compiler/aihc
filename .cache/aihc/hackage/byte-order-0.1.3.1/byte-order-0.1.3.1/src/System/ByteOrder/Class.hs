{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module System.ByteOrder.Class
  ( FixedOrdering (..)
  , Bytes (..)
  ) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.WideWord (Word128 (Word128), Word256 (Word256))
import Data.Word (Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian), targetByteOrder)
import GHC.Word (Word (W#))

import qualified GHC.Exts as Exts

{- | Types that are represented as a fixed-sized word. For these
types, the bytes can be swapped. The instances of this class
use byteswapping primitives and compile-time knowledge of native
endianness to provide portable endianness conversion functions.
-}
class Bytes a where
  -- | Convert from a native-endian word to a big-endian word.
  toBigEndian :: a -> a

  -- | Convert from a native-endian word to a little-endian word.
  toLittleEndian :: a -> a

instance Bytes Word8 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = id
  toLittleEndian = id

instance Bytes Word16 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian -> byteSwap16
  toLittleEndian = case targetByteOrder of
    BigEndian -> byteSwap16
    LittleEndian -> id

instance Bytes Word32 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian -> byteSwap32
  toLittleEndian = case targetByteOrder of
    BigEndian -> byteSwap32
    LittleEndian -> id

instance Bytes Word64 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian -> byteSwap64
  toLittleEndian = case targetByteOrder of
    BigEndian -> byteSwap64
    LittleEndian -> id

instance Bytes Word128 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian -> (\(Word128 hi lo) -> Word128 (byteSwap64 lo) (byteSwap64 hi))
  toLittleEndian = case targetByteOrder of
    BigEndian -> (\(Word128 hi lo) -> Word128 (byteSwap64 lo) (byteSwap64 hi))
    LittleEndian -> id

instance Bytes Word256 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian -> (\(Word256 a b c d) -> Word256 (byteSwap64 d) (byteSwap64 c) (byteSwap64 b) (byteSwap64 a))
  toLittleEndian = case targetByteOrder of
    BigEndian -> (\(Word256 a b c d) -> Word256 (byteSwap64 d) (byteSwap64 c) (byteSwap64 b) (byteSwap64 a))
    LittleEndian -> id

instance Bytes Word where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian -> byteSwap
  toLittleEndian = case targetByteOrder of
    BigEndian -> byteSwap
    LittleEndian -> id

instance Bytes Int8 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = id
  toLittleEndian = id

instance Bytes Int16 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian ->
      fromIntegral @Word16 @Int16
        . byteSwap16
        . fromIntegral @Int16 @Word16
  toLittleEndian = case targetByteOrder of
    BigEndian ->
      fromIntegral @Word16 @Int16
        . byteSwap16
        . fromIntegral @Int16 @Word16
    LittleEndian -> id

instance Bytes Int32 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian ->
      fromIntegral @Word32 @Int32
        . byteSwap32
        . fromIntegral @Int32 @Word32
  toLittleEndian = case targetByteOrder of
    BigEndian ->
      fromIntegral @Word32 @Int32
        . byteSwap32
        . fromIntegral @Int32 @Word32
    LittleEndian -> id

instance Bytes Int64 where
  {-# INLINE toBigEndian #-}
  {-# INLINE toLittleEndian #-}
  toBigEndian = case targetByteOrder of
    BigEndian -> id
    LittleEndian ->
      fromIntegral @Word64 @Int64
        . byteSwap64
        . fromIntegral @Int64 @Word64
  toLittleEndian = case targetByteOrder of
    BigEndian ->
      fromIntegral @Word64 @Int64
        . byteSwap64
        . fromIntegral @Int64 @Word64
    LittleEndian -> id

{- | A byte order that can be interpreted as a conversion function.
This class is effectively closed. The only instances are for
'BigEndian' and 'LittleEndian'. It is not possible to write more
instances since there are no other inhabitants of 'ByteOrder'.
-}
class FixedOrdering (b :: ByteOrder) where
  toFixedEndian :: (Bytes a) => a -> a

instance FixedOrdering 'LittleEndian where
  toFixedEndian = toLittleEndian

instance FixedOrdering 'BigEndian where
  toFixedEndian = toBigEndian

byteSwap :: Word -> Word
{-# INLINE byteSwap #-}
byteSwap (W# w) = W# (Exts.byteSwap# w)
