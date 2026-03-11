{-# LANGUAGE CPP #-}

module Ptr.IO where

import qualified Data.ByteString.Internal as A
import qualified Data.ByteString.Short.Internal as B
import Ptr.Prelude
import qualified Ptr.UncheckedShifting as D

{-# INLINE peekStorable #-}
peekStorable :: (Storable storable) => Ptr Word8 -> IO storable
peekStorable =
  peek . castPtr

{-# INLINE peekWord8 #-}
peekWord8 :: Ptr Word8 -> IO Word8
peekWord8 =
  peekStorable

-- | Big-endian word of 2 bytes.
{-# INLINE peekBEWord16 #-}
peekBEWord16 :: Ptr Word8 -> IO Word16
#ifdef WORDS_BIGENDIAN
peekBEWord16 =
  peekStorable
#else
peekBEWord16 =
  fmap byteSwap16 . peekStorable
#endif

-- | Little-endian word of 2 bytes.
{-# INLINE peekLEWord16 #-}
peekLEWord16 :: Ptr Word8 -> IO Word16
#ifdef WORDS_BIGENDIAN
peekLEWord16 =
  fmap byteSwap16 . peekStorable
#else
peekLEWord16 =
  peekStorable
#endif

-- | Big-endian word of 4 bytes.
{-# INLINE peekBEWord32 #-}
peekBEWord32 :: Ptr Word8 -> IO Word32
#ifdef WORDS_BIGENDIAN
peekBEWord32 =
  peekStorable
#else
peekBEWord32 =
  fmap byteSwap32 . peekStorable
#endif

-- | Little-endian word of 4 bytes.
{-# INLINE peekLEWord32 #-}
peekLEWord32 :: Ptr Word8 -> IO Word32
#ifdef WORDS_BIGENDIAN
peekLEWord32 =
  fmap byteSwap32 . peekStorable
#else
peekLEWord32 =
  peekStorable
#endif

-- | Big-endian word of 8 bytes.
{-# INLINE peekBEWord64 #-}
peekBEWord64 :: Ptr Word8 -> IO Word64
#ifdef WORDS_BIGENDIAN
peekBEWord64 =
  peekStorable
#else
peekBEWord64 =
  fmap byteSwap64 . peekStorable
#endif

-- | Little-endian word of 8 bytes.
{-# INLINE peekLEWord64 #-}
peekLEWord64 :: Ptr Word8 -> IO Word64
#ifdef WORDS_BIGENDIAN
peekLEWord64 =
  fmap byteSwap64 . peekStorable
#else
peekLEWord64 =
  peekStorable
#endif

{-# INLINE peekInt8 #-}
peekInt8 :: Ptr Word8 -> IO Int8
peekInt8 =
  peekStorable

-- | Big-endian int of 2 bytes.
{-# INLINE peekBEInt16 #-}
peekBEInt16 :: Ptr Word8 -> IO Int16
#ifdef WORDS_BIGENDIAN
peekBEInt16 =
  peekStorable
#else
peekBEInt16 =
  fmap (fromIntegral . byteSwap16) . peekStorable
#endif

-- | Little-endian int of 2 bytes.
{-# INLINE peekLEInt16 #-}
peekLEInt16 :: Ptr Word8 -> IO Int16
#ifdef WORDS_BIGENDIAN
peekLEInt16 =
  fmap (fromIntegral . byteSwap16) . peekStorable
#else
peekLEInt16 =
  peekStorable
#endif

-- | Big-endian int of 4 bytes.
{-# INLINE peekBEInt32 #-}
peekBEInt32 :: Ptr Word8 -> IO Int32
#ifdef WORDS_BIGENDIAN
peekBEInt32 =
  peekStorable
#else
peekBEInt32 =
  fmap (fromIntegral . byteSwap32) . peekStorable
#endif

-- | Little-endian int of 4 bytes.
{-# INLINE peekLEInt32 #-}
peekLEInt32 :: Ptr Word8 -> IO Int32
#ifdef WORDS_BIGENDIAN
peekLEInt32 =
  fmap (fromIntegral . byteSwap32) . peekStorable
#else
peekLEInt32 =
  peekStorable
#endif

-- | Big-endian int of 8 bytes.
{-# INLINE peekBEInt64 #-}
peekBEInt64 :: Ptr Word8 -> IO Int64
#ifdef WORDS_BIGENDIAN
peekBEInt64 =
  peekStorable
#else
peekBEInt64 =
  fmap (fromIntegral . byteSwap64) . peekStorable
#endif

-- | Little-endian int of 8 bytes.
{-# INLINE peekLEInt64 #-}
peekLEInt64 :: Ptr Word8 -> IO Int64
#ifdef WORDS_BIGENDIAN
peekLEInt64 =
  fmap (fromIntegral . byteSwap64) . peekStorable
#else
peekLEInt64 =
  peekStorable
#endif

-- |
-- Allocate a new byte array with @copyBytes@.
{-# INLINE peekBytes #-}
peekBytes :: Ptr Word8 -> Int -> IO ByteString
peekBytes ptr amount =
  A.create amount $ \destPtr -> copyBytes destPtr ptr amount

{-# INLINE peekShortByteString #-}
peekShortByteString :: Ptr Word8 -> Int -> IO ShortByteString
peekShortByteString ptr amount =
  B.createFromPtr ptr amount

{-# INLINE peekNullTerminatedShortByteString #-}
peekNullTerminatedShortByteString :: Ptr Word8 -> (Int -> IO ShortByteString -> IO a) -> IO a
peekNullTerminatedShortByteString ptr cont =
  do
    !length <- fromIntegral <$> A.c_strlen (castPtr ptr)
    cont length (B.createFromPtr ptr length)

{-# INLINE pokeStorable #-}
pokeStorable :: (Storable a) => Ptr Word8 -> a -> IO ()
pokeStorable =
  poke . castPtr

{-# INLINE pokeStorableByteOff #-}
pokeStorableByteOff :: (Storable a) => Ptr Word8 -> Int -> a -> IO ()
pokeStorableByteOff =
  pokeByteOff . castPtr

{-# INLINE pokeWord8 #-}
pokeWord8 :: Ptr Word8 -> Word8 -> IO ()
pokeWord8 ptr value =
  poke ptr value

{-# INLINE pokeWord8Off #-}
pokeWord8Off :: Ptr Word8 -> Int -> Word8 -> IO ()
pokeWord8Off ptr off value =
  pokeByteOff ptr off value

{-# INLINE pokeBEWord16 #-}
pokeBEWord16 :: Ptr Word8 -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord16 =
  pokeStorable
#else
pokeBEWord16 ptr value =
  do
    pokeStorable ptr (fromIntegral (D.shiftr_w16 value 8) :: Word8)
    pokeByteOff ptr 1 (fromIntegral value :: Word8)
#endif

{-# INLINE pokeBEWord16ByteOff #-}
pokeBEWord16ByteOff :: Ptr Word8 -> Int -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord16ByteOff =
  pokeStorableByteOff
#else
pokeBEWord16ByteOff ptr off value =
  do
    pokeStorableByteOff ptr off (fromIntegral (D.shiftr_w16 value 8) :: Word8)
    pokeByteOff ptr (off + 1) (fromIntegral value :: Word8)
#endif

{-# INLINE pokeBEWord32 #-}
pokeBEWord32 :: Ptr Word8 -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord32 =
  pokeStorable
#else
pokeBEWord32 ptr value =
  do
    pokeStorable ptr (fromIntegral (D.shiftr_w32 value 24) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (D.shiftr_w32 value 16) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (D.shiftr_w32 value 8) :: Word8)
    pokeByteOff ptr 3 (fromIntegral value :: Word8)
#endif

{-# INLINE pokeBEWord32ByteOff #-}
pokeBEWord32ByteOff :: Ptr Word8 -> Int -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord32ByteOff =
  pokeStorableByteOff
#else
pokeBEWord32ByteOff ptr off value =
  do
    pokeStorableByteOff ptr off (fromIntegral (D.shiftr_w32 value 24) :: Word8)
    pokeByteOff ptr (off + 1) (fromIntegral (D.shiftr_w32 value 16) :: Word8)
    pokeByteOff ptr (off + 2) (fromIntegral (D.shiftr_w32 value 8) :: Word8)
    pokeByteOff ptr (off + 3) (fromIntegral value :: Word8)
#endif

{-# INLINE pokeBEWord64 #-}
pokeBEWord64 :: Ptr Word8 -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord64 =
  pokeStorable
#else
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
pokeBEWord64 ptr value =
  do
    pokeBEWord32 ptr (fromIntegral (D.shiftr_w64 value 32))
    pokeBEWord32 (plusPtr ptr 4) (fromIntegral value)
#else
pokeBEWord64 ptr value =
  do
    pokeStorable ptr (fromIntegral (D.shiftr_w64 value 56) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (D.shiftr_w64 value 48) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (D.shiftr_w64 value 40) :: Word8)
    pokeByteOff ptr 3 (fromIntegral (D.shiftr_w64 value 32) :: Word8)
    pokeByteOff ptr 4 (fromIntegral (D.shiftr_w64 value 24) :: Word8)
    pokeByteOff ptr 5 (fromIntegral (D.shiftr_w64 value 16) :: Word8)
    pokeByteOff ptr 6 (fromIntegral (D.shiftr_w64 value  8) :: Word8)
    pokeByteOff ptr 7 (fromIntegral value :: Word8)
#endif
#endif

{-# INLINE pokeBEWord64ByteOff #-}
pokeBEWord64ByteOff :: Ptr Word8 -> Int -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord64ByteOff =
  pokeStorableByteOff
#else
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
pokeBEWord64ByteOff ptr off value =
  do
    pokeBEWord32ByteOff ptr off (fromIntegral (D.shiftr_w64 value 32))
    pokeBEWord32ByteOff ptr (off + 4) (fromIntegral value)
#else
pokeBEWord64ByteOff ptr off value =
  do
    pokeStorableByteOff ptr off (fromIntegral (D.shiftr_w64 value 56) :: Word8)
    pokeByteOff ptr (off + 1) (fromIntegral (D.shiftr_w64 value 48) :: Word8)
    pokeByteOff ptr (off + 2) (fromIntegral (D.shiftr_w64 value 40) :: Word8)
    pokeByteOff ptr (off + 3) (fromIntegral (D.shiftr_w64 value 32) :: Word8)
    pokeByteOff ptr (off + 4) (fromIntegral (D.shiftr_w64 value 24) :: Word8)
    pokeByteOff ptr (off + 5) (fromIntegral (D.shiftr_w64 value 16) :: Word8)
    pokeByteOff ptr (off + 6) (fromIntegral (D.shiftr_w64 value  8) :: Word8)
    pokeByteOff ptr (off + 7) (fromIntegral value :: Word8)
#endif
#endif

{-# INLINE pokeLEWord16 #-}
pokeLEWord16 :: Ptr Word8 -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeLEWord16 p w =
  do
    pokeWord8 p (fromIntegral w)
    pokeWord8Off p 1 (fromIntegral (D.shiftr_w16 w 8))
#else
pokeLEWord16 =
  pokeStorable
#endif

{-# INLINE pokeLEWord32 #-}
pokeLEWord32 :: Ptr Word8 -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeLEWord32 p w =
  do
    pokeWord8 p (fromIntegral w)
    pokeWord8Off p 1 (fromIntegral (D.shiftr_w32 w 8))
    pokeWord8Off p 2 (fromIntegral (D.shiftr_w32 w 16))
    pokeWord8Off p 3 (fromIntegral (D.shiftr_w32 w 24))
#else
pokeLEWord32 =
  pokeStorable
#endif

{-# INLINE pokeLEWord64 #-}
pokeLEWord64 :: Ptr Word8 -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
pokeLEWord64 p w =
  do
    let b = fromIntegral (D.shiftr_w64 w 32) :: Word32
        a = fromIntegral w                   :: Word32
    pokeWord8 p (fromIntegral a)
    pokeWord8Off p 1 (fromIntegral (D.shiftr_w32 a 8))
    pokeWord8Off p 2 (fromIntegral (D.shiftr_w32 a 16))
    pokeWord8Off p 3 (fromIntegral (D.shiftr_w32 a 24))
    pokeWord8Off p 4 (fromIntegral b)
    pokeWord8Off p 5 (fromIntegral (D.shiftr_w32 b 8))
    pokeWord8Off p 6 (fromIntegral (D.shiftr_w32 b 16))
    pokeWord8Off p 7 (fromIntegral (D.shiftr_w32 b 24))
#else
pokeLEWord64 p w =
  do
    pokeWord8 p (fromIntegral w)
    pokeWord8Off p 1 (fromIntegral (D.shiftr_w64 w 8))
    pokeWord8Off p 2 (fromIntegral (D.shiftr_w64 w 16))
    pokeWord8Off p 3 (fromIntegral (D.shiftr_w64 w 24))
    pokeWord8Off p 4 (fromIntegral (D.shiftr_w64 w 32))
    pokeWord8Off p 5 (fromIntegral (D.shiftr_w64 w 40))
    pokeWord8Off p 6 (fromIntegral (D.shiftr_w64 w 48))
    pokeWord8Off p 7 (fromIntegral (D.shiftr_w64 w 56))
#endif
#else
pokeLEWord64 =
  pokeStorable
#endif

{-# INLINE pokeBytesTrimming #-}
pokeBytesTrimming :: Ptr Word8 -> Int -> ByteString -> IO ()
pokeBytesTrimming ptr maxLength (A.PS fptr offset length) =
  withForeignPtr fptr $ \bytesPtr -> copyBytes ptr (plusPtr bytesPtr offset) (min length maxLength)

{-# INLINE pokeBytes #-}
pokeBytes :: Ptr Word8 -> ByteString -> IO ()
#if MIN_VERSION_bytestring(0,11,0)
pokeBytes ptr (A.BS fptr length) =
  withForeignPtr fptr $ \bytesPtr -> copyBytes ptr bytesPtr length
#else
pokeBytes ptr (A.PS fptr offset length) =
  withForeignPtr fptr $ \bytesPtr -> copyBytes ptr (plusPtr bytesPtr offset) length
#endif
