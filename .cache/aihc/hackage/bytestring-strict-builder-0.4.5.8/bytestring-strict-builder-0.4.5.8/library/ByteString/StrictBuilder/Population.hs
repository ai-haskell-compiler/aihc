{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module ByteString.StrictBuilder.Population where

import qualified ByteString.StrictBuilder.Population.UncheckedShifting as D
import ByteString.StrictBuilder.Prelude
import qualified ByteString.StrictBuilder.UTF8 as E
import qualified Data.ByteString.Builder.Internal as G
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Internal as C
import Foreign.Marshal.Utils

newtype Population = Population {populationPtrUpdate :: Ptr Word8 -> IO (Ptr Word8)}

instance Semigroup Population where
  (<>) (Population leftPtrUpdate) (Population rightPtrUpdate) =
    Population (leftPtrUpdate >=> rightPtrUpdate)

instance Monoid Population where
  {-# INLINE mempty #-}
  mempty =
    Population return

-- |
-- Turns into the standard lazy bytestring builder.
{-# INLINE populationChunksBuilder #-}
populationChunksBuilder :: Population -> G.Builder
populationChunksBuilder (Population ptrUpdate) =
  G.builder stepUpdate
  where
    stepUpdate :: G.BuildStep a -> G.BuildStep a
    stepUpdate nextStep (G.BufferRange beginningPtr afterPtr) =
      do
        newBeginningPtr <- ptrUpdate beginningPtr
        nextStep $! G.BufferRange newBeginningPtr afterPtr

{-# INLINE followParallelly #-}
followParallelly :: Population -> Int -> Population -> Population
followParallelly (Population followerPtrUpdate) followeeLength (Population followeePtrUpdate) =
  Population ptrUpdate
  where
    ptrUpdate ptr =
      do
        lock <- newEmptyMVar
        forkIO $ do
          followeePtrUpdate ptr
          putMVar lock ()
        followerPtrUpdate (plusPtr ptr followeeLength) <* takeMVar lock

-- |
-- Write the given bytes into the pointer and
-- return a pointer incremented by the amount of written bytes.
{-# INLINE bytes #-}
bytes :: B.ByteString -> Population
bytes (B.PS foreignPointer offset length) =
  Population $ \ptr ->
    withForeignPtr foreignPointer $ \ptr' ->
      copyBytes ptr (plusPtr ptr' offset) length $> plusPtr ptr length

{-# INLINE storable #-}
storable :: (Storable a) => Int -> a -> Population
storable size value =
  Population (\ptr -> poke (castPtr ptr) value $> plusPtr ptr size)

{-# INLINE word8 #-}
word8 :: Word8 -> Population
word8 value =
  Population $ \ptr ->
    poke ptr value $> plusPtr ptr 1

{-# INLINE word16BE #-}
word16BE :: Word16 -> Population
#ifdef WORDS_BIGENDIAN
word16BE =
  storable 2
#else
word16BE value =
  Population $ \ptr -> do
    poke ptr (fromIntegral (D.shiftr_w16 value 8) :: Word8)
    pokeByteOff ptr 1 (fromIntegral value :: Word8)
    return (plusPtr ptr 2)
#endif

{-# INLINE word32BE #-}
word32BE :: Word32 -> Population
#ifdef WORDS_BIGENDIAN
word32BE =
  storable 4
#else
word32BE value =
  Population $ \ptr -> do
    poke ptr (fromIntegral (D.shiftr_w32 value 24) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (D.shiftr_w32 value 16) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (D.shiftr_w32 value 8) :: Word8)
    pokeByteOff ptr 3 (fromIntegral value :: Word8)
    return (plusPtr ptr 4)
#endif

{-# INLINE word64BE #-}
word64BE :: Word64 -> Population
#ifdef WORDS_BIGENDIAN
word64BE =
  storable 8
#else
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
word64BE value =
  word32BE (fromIntegral (D.shiftr_w64 value 32)) <>
  word32BE (fromIntegral value)
#else
word64BE value =
  Population $ \ptr -> do
    poke ptr (fromIntegral (D.shiftr_w64 value 56) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (D.shiftr_w64 value 48) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (D.shiftr_w64 value 40) :: Word8)
    pokeByteOff ptr 3 (fromIntegral (D.shiftr_w64 value 32) :: Word8)
    pokeByteOff ptr 4 (fromIntegral (D.shiftr_w64 value 24) :: Word8)
    pokeByteOff ptr 5 (fromIntegral (D.shiftr_w64 value 16) :: Word8)
    pokeByteOff ptr 6 (fromIntegral (D.shiftr_w64 value  8) :: Word8)
    pokeByteOff ptr 7 (fromIntegral value :: Word8)
    return (plusPtr ptr 8)
#endif
#endif

{-# INLINE int8 #-}
int8 :: Int8 -> Population
int8 =
  word8 . fromIntegral

-- | Encoding 'Int16's in big endian format.
{-# INLINE int16BE #-}
int16BE :: Int16 -> Population
int16BE =
  word16BE . fromIntegral

-- | Encoding 'Int32's in big endian format.
{-# INLINE int32BE #-}
int32BE :: Int32 -> Population
int32BE =
  word32BE . fromIntegral

-- | Encoding 'Int64's in big endian format.
{-# INLINE int64BE #-}
int64BE :: Int64 -> Population
int64BE =
  word64BE . fromIntegral

{-# INLINE bytes_1 #-}
bytes_1 :: Word8 -> Population
bytes_1 byte1 =
  Population $ \ptr -> do
    poke ptr byte1
    return (plusPtr ptr 1)

{-# INLINE bytes_2 #-}
bytes_2 :: Word8 -> Word8 -> Population
bytes_2 byte1 byte2 =
  Population $ \ptr -> do
    poke ptr byte1
    pokeByteOff ptr 1 byte2
    return (plusPtr ptr 2)

{-# INLINE bytes_3 #-}
bytes_3 :: Word8 -> Word8 -> Word8 -> Population
bytes_3 byte1 byte2 byte3 =
  Population $ \ptr -> do
    poke ptr byte1
    pokeByteOff ptr 1 byte2
    pokeByteOff ptr 2 byte3
    return (plusPtr ptr 3)

{-# INLINE bytes_4 #-}
bytes_4 :: Word8 -> Word8 -> Word8 -> Word8 -> Population
bytes_4 byte1 byte2 byte3 byte4 =
  Population $ \ptr -> do
    poke ptr byte1
    pokeByteOff ptr 1 byte2
    pokeByteOff ptr 2 byte3
    pokeByteOff ptr 3 byte4
    return (plusPtr ptr 4)

{-# INLINE utf8UnicodeCodePoint #-}
utf8UnicodeCodePoint :: Int -> Population
utf8UnicodeCodePoint x =
  E.unicodeCodePoint x bytes_1 bytes_2 bytes_3 bytes_4

{-# INLINE utf8Char #-}
utf8Char :: Int -> Population
utf8Char x =
  E.unicodeCodePoint x bytes_1 bytes_2 bytes_3 bytes_4
