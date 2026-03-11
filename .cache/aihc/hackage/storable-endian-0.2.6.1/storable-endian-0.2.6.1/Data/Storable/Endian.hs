{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Storable.Endian
-- Copyright   :  (c) Eugene Kirpichov 2010
-- License     :  New BSD
--
-- Maintainer  :  Eugene Kirpichov <ekirpichov@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Storable instances with endianness.
--

module Data.Storable.Endian
  (
    LittleEndian(..), BigEndian(..),
    HasLittleEndian(..), HasBigEndian(..)
  )
  where

import System.ByteOrder

import Foreign.Ptr
import Foreign.Storable

import Unsafe.Coerce

import Data.Bits
import Data.Word
import Data.Int

import GHC.Generics (Generic(..))

newtype LittleEndian a = LE { getLittleEndian :: a }
  deriving ( Eq, Ord, Show, Read, Enum
           , Num, Integral, Fractional, Floating, Real, RealFrac, RealFloat
           , Generic)

newtype BigEndian    a = BE { getBigEndian    :: a }
  deriving ( Eq, Ord, Show, Read, Enum
           , Num, Integral, Fractional, Floating, Real, RealFrac, RealFloat
           , Generic)

class HasLittleEndian a where
  peekLE :: Ptr a -> IO a
  pokeLE :: Ptr a -> a -> IO ()

class HasBigEndian a where
  peekBE :: Ptr a -> IO a
  pokeBE :: Ptr a -> a -> IO ()

instance (HasLittleEndian a, Storable a) => Storable (LittleEndian a) where
  sizeOf    (LE a)   = sizeOf a
  alignment (LE a)   = alignment a
  peek      p        = LE `fmap` peekLE (castPtr p)
  poke      p (LE a) = pokeLE (castPtr p) a

instance (HasBigEndian a, Storable a) => Storable (BigEndian a) where
  sizeOf    (BE a)   = sizeOf a
  alignment (BE a)   = alignment a
  peek      p        = BE `fmap` peekBE (castPtr p)
  poke      p (BE a) = pokeBE (castPtr p) a

fork :: (a1 -> b1 -> c) -> (a0 -> a1) -> (b0 -> b1) -> a0 -> b0 -> c
fork f onX onY = \x y -> f (onX x) (onY y)

------------------------------
-- Little-endian instances
------------------------------

instance HasLittleEndian Int16 where
  peekLE = unsafeCoerce . getWord16le . castPtr
  pokeLE = fork putWord16le castPtr unsafeCoerce

instance HasLittleEndian Int32 where
  peekLE = unsafeCoerce . getWord32le . castPtr
  pokeLE = fork putWord32le castPtr unsafeCoerce

instance HasLittleEndian Int64 where
  peekLE = unsafeCoerce . getWord64le . castPtr
  pokeLE = fork putWord64le castPtr unsafeCoerce

instance HasLittleEndian Word16 where
  peekLE = unsafeCoerce . getWord16le . castPtr
  pokeLE = fork putWord16le castPtr unsafeCoerce

instance HasLittleEndian Word32 where
  peekLE = unsafeCoerce . getWord32le . castPtr
  pokeLE = fork putWord32le castPtr unsafeCoerce

instance HasLittleEndian Word64 where
  peekLE = unsafeCoerce . getWord64le . castPtr
  pokeLE = fork putWord64le castPtr unsafeCoerce

instance HasLittleEndian Float where
  peekLE = unsafeCoerce . getWord32le . castPtr
  pokeLE = fork putWord32le castPtr unsafeCoerce

instance HasLittleEndian Double where
  peekLE = unsafeCoerce . getWord64le . castPtr
  pokeLE = fork putWord64le castPtr unsafeCoerce


------------------------------
-- Big-endian instances
------------------------------

instance HasBigEndian Int16 where
  peekBE = unsafeCoerce . getWord16be . castPtr
  pokeBE = fork putWord16be castPtr unsafeCoerce

instance HasBigEndian Int32 where
  peekBE = unsafeCoerce . getWord32be . castPtr
  pokeBE = fork putWord32be castPtr unsafeCoerce

instance HasBigEndian Int64 where
  peekBE = unsafeCoerce . getWord64be . castPtr
  pokeBE = fork putWord64be castPtr unsafeCoerce

instance HasBigEndian Word16 where
  peekBE = unsafeCoerce . getWord16be . castPtr
  pokeBE = fork putWord16be castPtr unsafeCoerce

instance HasBigEndian Word32 where
  peekBE = unsafeCoerce . getWord32be . castPtr
  pokeBE = fork putWord32be castPtr unsafeCoerce

instance HasBigEndian Word64 where
  peekBE = unsafeCoerce . getWord64be . castPtr
  pokeBE = fork putWord64be castPtr unsafeCoerce

instance HasBigEndian Float where
  peekBE = unsafeCoerce . getWord32be . castPtr
  pokeBE = fork putWord32be castPtr unsafeCoerce

instance HasBigEndian Double where
  peekBE = unsafeCoerce . getWord64be . castPtr
  pokeBE = fork putWord64be castPtr unsafeCoerce


-- The code below has been adapted from the 'binary' library
-- http://hackage.haskell.org/package/binary

------------------------------------------------------------------------

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a Word16 in big endian format
putWord16be :: Ptr Word8 -> Word16 -> IO ()
putWord16be = if byteOrder == BigEndian
              then \p w -> poke (castPtr p) w
              else \p w -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le :: Ptr Word8 -> Word16 -> IO ()
putWord16le = if byteOrder == LittleEndian
              then \p w -> poke (castPtr p) w
              else \p w -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
{-# INLINE putWord16le #-}

-- putWord16le w16 = writeN 2 (\p -> poke (castPtr p) w16)

-- | Write a Word32 in big endian format
putWord32be :: Ptr Word8 -> Word32 -> IO ()
putWord32be = if byteOrder == BigEndian
              then \p w -> poke (castPtr p) w
              else \p w -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32le :: Ptr Word8 -> Word32 -> IO ()
putWord32le = if byteOrder == LittleEndian
              then \p w -> poke (castPtr p) w
              else \p w -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
putWord64be :: Ptr Word8 -> Word64 -> IO ()
putWord64be = if byteOrder == BigEndian
              then \p w -> poke (castPtr p) w
              else \p w -> do
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
    let a = fromIntegral (shiftr_w64 w 32) :: Word32
        b = fromIntegral w                 :: Word32
    poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
#endif
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le :: Ptr Word8 -> Word64 -> IO ()
putWord64le = if byteOrder == LittleEndian
              then \p w -> poke (castPtr p) w
              else \p w -> do
#if WORD_SIZE_IN_BITS < 64
    let b = fromIntegral (shiftr_w64 w 32) :: Word32
        a = fromIntegral w                 :: Word32
    poke (p)             (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
#endif
{-# INLINE putWord64le #-}

-- on a little endian machine:
-- putWord64le w64 = writeN 8 (\p -> poke (castPtr p) w64)
-- | Read a Word16 in big endian format
getWord16be :: Ptr Word8 -> IO Word16
getWord16be = if byteOrder == BigEndian
              then \p -> peek (castPtr p)
              else \p -> do
    b0 <- fromIntegral `fmap` (p `peekElemOff` 0)
    b1 <- fromIntegral `fmap` (p `peekElemOff` 1)
    return $! (b0 `shiftl_w16` 8) .|.
              (b1 )
{-# INLINE getWord16be #-}

-- | Read a Word16 in little endian format
getWord16le :: Ptr Word8 -> IO Word16
getWord16le = if byteOrder == LittleEndian
              then \p -> peek (castPtr p)
              else \p -> do
    b0 <- fromIntegral `fmap` (p `peekElemOff` 0)
    b1 <- fromIntegral `fmap` (p `peekElemOff` 1)
    return $! (b1 `shiftl_w16` 8) .|.
              (b0 )
{-# INLINE getWord16le #-}

-- | Read a Word32 in big endian format
getWord32be :: Ptr Word8 -> IO Word32
getWord32be = if byteOrder == BigEndian
              then \p -> peek (castPtr p)
              else \p -> do
    b0 <- fromIntegral `fmap` (p `peekElemOff` 0)
    b1 <- fromIntegral `fmap` (p `peekElemOff` 1)
    b2 <- fromIntegral `fmap` (p `peekElemOff` 2)
    b3 <- fromIntegral `fmap` (p `peekElemOff` 3)
    return $! (b0 `shiftl_w32` 24) .|.
              (b1 `shiftl_w32` 16) .|.
              (b2 `shiftl_w32`  8) .|.
              (b3 )
{-# INLINE getWord32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Ptr Word8 -> IO Word32
getWord32le = if byteOrder == LittleEndian
              then \p -> peek (castPtr p)
              else \p -> do
    b0 <- fromIntegral `fmap` (p `peekElemOff` 0)
    b1 <- fromIntegral `fmap` (p `peekElemOff` 1)
    b2 <- fromIntegral `fmap` (p `peekElemOff` 2)
    b3 <- fromIntegral `fmap` (p `peekElemOff` 3)
    return $! (b3 `shiftl_w32` 24) .|.
              (b2 `shiftl_w32` 16) .|.
              (b1 `shiftl_w32`  8) .|.
              (b0 )
{-# INLINE getWord32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Ptr Word8 -> IO Word64
getWord64be = if byteOrder == BigEndian
              then \p -> peek (castPtr p)
              else \p -> do
    b0 <- fromIntegral `fmap` (p `peekElemOff` 0)
    b1 <- fromIntegral `fmap` (p `peekElemOff` 1)
    b2 <- fromIntegral `fmap` (p `peekElemOff` 2)
    b3 <- fromIntegral `fmap` (p `peekElemOff` 3)
    b4 <- fromIntegral `fmap` (p `peekElemOff` 4)
    b5 <- fromIntegral `fmap` (p `peekElemOff` 5)
    b6 <- fromIntegral `fmap` (p `peekElemOff` 6)
    b7 <- fromIntegral `fmap` (p `peekElemOff` 7)
    return $! (b0 `shiftl_w64` 56) .|.
              (b1 `shiftl_w64` 48) .|.
              (b2 `shiftl_w64` 40) .|.
              (b3 `shiftl_w64` 32) .|.
              (b4 `shiftl_w64` 24) .|.
              (b5 `shiftl_w64` 16) .|.
              (b6 `shiftl_w64`  8) .|.
              (b7 )
{-# INLINE getWord64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Ptr Word8 -> IO Word64
getWord64le = if byteOrder == LittleEndian
              then \p -> peek (castPtr p)
              else \p -> do
    b0 <- fromIntegral `fmap` (p `peekElemOff` 0)
    b1 <- fromIntegral `fmap` (p `peekElemOff` 1)
    b2 <- fromIntegral `fmap` (p `peekElemOff` 2)
    b3 <- fromIntegral `fmap` (p `peekElemOff` 3)
    b4 <- fromIntegral `fmap` (p `peekElemOff` 4)
    b5 <- fromIntegral `fmap` (p `peekElemOff` 5)
    b6 <- fromIntegral `fmap` (p `peekElemOff` 6)
    b7 <- fromIntegral `fmap` (p `peekElemOff` 7)
    return $! (b7 `shiftl_w64` 56) .|.
              (b6 `shiftl_w64` 48) .|.
              (b5 `shiftl_w64` 40) .|.
              (b4 `shiftl_w64` 32) .|.
              (b3 `shiftl_w64` 24) .|.
              (b2 `shiftl_w64` 16) .|.
              (b1 `shiftl_w64`  8) .|.
              (b0 )
{-# INLINE getWord64le #-}

------------------------------------------------------------------------
-- Unchecked shifts

{-# INLINE shiftr_w16 #-}
shiftr_w16 :: Word16 -> Int -> Word16
{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32
{-# INLINE shiftr_w64 #-}
shiftr_w64 :: Word64 -> Int -> Word64

shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR

------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
