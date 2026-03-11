{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE Haskell2010   #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

{-# LANGUAGE Trustworthy   #-}

-- |
-- Copyright: © 2020  Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Apply XOR-masks to 'BS.ByteString's and memory regions.
--
module Data.XOR
    ( -- * Apply 32-bit XOR mask
      xor32StrictByteString
    , xor32StrictByteString'
    , xor32LazyByteString
    , xor32ShortByteString
    , xor32CStringLen

      -- * Apply 8-bit XOR mask
    , xor8StrictByteString
    , xor8LazyByteString
    , xor8ShortByteString
    , xor8CStringLen

    ) where

-- base
import           Control.Exception              (assert)
import           Control.Monad                  (void)
import           Control.Monad.ST               (ST, runST)
import           Data.Bits
import           Data.Tuple                     (swap)
import           Endianness                     (ByteOrder (..), Word32, Word8, byteSwap32,
                                                 targetByteOrder)
import           Foreign.C                      (CStringLen)
import           Foreign.ForeignPtr             (withForeignPtr)
import           Foreign.Marshal.Utils          (copyBytes)
import           Foreign.Ptr                    (Ptr, alignPtr, castPtr, minusPtr, plusPtr)
import           Foreign.Storable               (peek, poke)
import           System.IO.Unsafe               (unsafeDupablePerformIO)

import qualified GHC.Exts                       as X
import qualified GHC.ST                         as X
import qualified GHC.Word                       as X

-- bytestring
import qualified Data.ByteString                as BS
import           Data.ByteString.Internal       (mallocByteString)
import qualified Data.ByteString.Internal       as BS (ByteString (..))
import qualified Data.ByteString.Lazy.Internal  as BL (ByteString (..))
import qualified Data.ByteString.Short          as SBS
import           Data.ByteString.Short.Internal (ShortByteString (SBS))

----------------------------------------------------------------------------

{- high-level reference impl

-- about 6-7 times slower
xor32StrictByteString'ref :: Word32 -> BS.ByteString -> BS.ByteString
xor32StrictByteString'ref 0    = id
xor32StrictByteString'ref msk0 = snd . BS.mapAccumL go msk0
  where
    go :: Word32 -> Word8 -> (Word32,Word8)
    go msk b = let b'   = fromIntegral msk' `xor` b
                   msk' = rotateL msk 8
               in b' `seq` (msk',b')

-- about 3 times slower
xor8StrictByteString'ref :: Word8 -> BS.ByteString -> BS.ByteString
xor8StrictByteString'ref 0    = id
xor8StrictByteString'ref msk0 = BS.map (xor msk0)

-}

-- | Apply 32-bit XOR mask (considered as four octets in big-endian order) to 'BS.ByteString'.
--
-- >>> xor32StrictByteString 0x37fa213d "\x7f\x9f\x4d\x51\x58"
-- "Hello"
--
-- In other words, the 32-bit word @0x37fa213d@ is taken as the infinite series of octets @('cycle' [0x37,0xfa,0x21,0x3d])@ and 'xor'ed with the respective octets from the input 'BS.ByteString'.
--
-- The 'xor' laws give rise to the following laws:
--
-- prop> xor32StrictByteString m (xor32StrictByteString m x) == x
--
-- prop> xor32StrictByteString 0 x == x
--
-- prop> xor32StrictByteString m (xor32StrictByteString n x) == xor32StrictByteString (m `xor` n) x
--
-- This function is semantically equivalent to the (less efficient) implementation shown below
--
-- > xor32StrictByteString'ref :: Word32 -> BS.ByteString -> BS.ByteString
-- > xor32StrictByteString'ref 0    = id
-- > xor32StrictByteString'ref msk0 = snd . BS.mapAccumL go msk0
-- >   where
-- >     go :: Word32 -> Word8 -> (Word32,Word8)
-- >     go msk b = let b'   = fromIntegral (msk' .&. 0xff) `xor` b
-- >                    msk' = rotateL msk 8
-- >                in (msk',b')
--
-- The 'xor32StrictByteString' implementation is about 6-7 times faster than the naive implementation above.
xor32StrictByteString :: Word32 -> BS.ByteString -> BS.ByteString
xor32StrictByteString 0 bs   = bs
xor32StrictByteString _ bs   | BS.null bs = bs
xor32StrictByteString msk bs = fst (xor32StrictByteString'' msk bs)

-- | Convenience version of 'xor32StrictByteString' which also returns the rotated XOR-mask useful for chained masking.
--
-- >>> xor32StrictByteString' 0x37fa213d "\x7f\x9f\x4d\x51\x58"
-- (0xfa213d37,"Hello")
--
xor32StrictByteString' :: Word32 -> BS.ByteString -> (Word32,BS.ByteString)
xor32StrictByteString' 0 bs   = (0,bs)
xor32StrictByteString' msk bs | BS.null bs = (msk,bs)
xor32StrictByteString' msk bs = swap (xor32StrictByteString'' msk bs)

-- | Variant of 'xor32StrictByteString' for masking lazy 'BL.ByteString's.
--
-- >>> xor32LazyByteString 0x37fa213d "\x7f\x9f\x4d\x51\x58"
-- "Hello"
--
xor32LazyByteString :: Word32 -> BL.ByteString -> BL.ByteString
xor32LazyByteString 0 = id
xor32LazyByteString msk0 = go msk0
  where
    go _ BL.Empty = BL.Empty
    go msk (BL.Chunk x xs) = BL.Chunk x' (go msk' xs)
      where
        (x',msk') = xor32StrictByteString'' msk x

{-# INLINE xor32StrictByteString'' #-}
-- internal
xor32StrictByteString'' :: Word32 -> BS.ByteString -> (BS.ByteString,Word32)
xor32StrictByteString'' msk0 (BS.PS x s l)
    = unsafeCreate' l $ \p8 ->
        withForeignPtr x $ \f -> do
          copyBytes p8 (f `plusPtr` s) (fromIntegral l)

          case remPtr p8 4 of
            0 -> do
              let trailer = l `rem` 4
                  lbytes = l - trailer
              xor32PtrAligned msk0 (castPtr p8) lbytes
              xor32PtrNonAligned msk0 (p8 `plusPtr` lbytes) trailer
            _ ->
              -- misaligned bytestring...
              --
              -- This should not happen, as newly allocated
              -- bytestrings ought to be word-aligned; but if the
              -- impossible does happen we have a semantically sound
              -- codepath to jump to...
              xor32Ptr msk0 p8 l



-- | Apply 32-bit XOR mask (considered as four octets in big-endian order) to 'SBS.ShortByteString'. See also 'xor32StrictByteString'.
--
-- >>> xor32ShortByteString 0x37fa213d "\x7f\x9f\x4d\x51\x58"
-- "Hello"
--
xor32ShortByteString :: Word32 -> SBS.ShortByteString -> SBS.ShortByteString
xor32ShortByteString 0 sbs = sbs
xor32ShortByteString _ sbs | SBS.null sbs = sbs
xor32ShortByteString mask0be sbs = runST $ do
    tmp <- newSBS len

    let loop4 i
          | i == len4  = return ()
          | otherwise  = writeWord32Array tmp i (indexWord32Array sbs i `xor` mask0) >> loop4 (i+1)

    loop4 0

    let writeXor8 ofs msk8 = writeWord8Array tmp ofs (indexWord8Array sbs ofs `xor` msk8)

    case len1 of
      0 -> return ()
      1 -> do
        writeXor8 (len-1) (fromIntegral (shiftR mask0be 24))
      2 -> do
        writeXor8 (len-2) (fromIntegral (shiftR mask0be 24))
        writeXor8 (len-1) (fromIntegral (shiftR mask0be 16))
      3 -> do
        writeXor8 (len-3) (fromIntegral (shiftR mask0be 24))
        writeXor8 (len-2) (fromIntegral (shiftR mask0be 16))
        writeXor8 (len-1) (fromIntegral (shiftR mask0be  8))
      _ -> undefined -- impossible

    unsafeFreezeSBS tmp
  where
    len = SBS.length sbs
    (len4,len1) = quotRem len 4

    mask0 = case targetByteOrder of
              LittleEndian -> byteSwap32 mask0be
              BigEndian    -> mask0be


{-# INLINEABLE xor32CStringLen #-}
-- | Apply 32-bit XOR mask (considered as four octets in big-endian order) to memory region expressed as base-pointer and size. The returned value is the input mask rotated by the word-size remained of the memory region size (useful for chained xor-masking of multiple memory-fragments).
xor32CStringLen :: Word32 -> CStringLen -> IO Word32
xor32CStringLen m (p,l) = xor32Ptr m (castPtr p) l

{-# INLINEABLE xor32Ptr #-}
xor32Ptr :: Word32 -> Ptr Word8 -> Int -> IO Word32
xor32Ptr 0      !_  !_ = return 0
xor32Ptr !mask0 !_   0 = return mask0
xor32Ptr !mask0 !p0 !n
  | n < 4 = xor32PtrNonAligned mask0 p0 n
  | n < 0 = fail "xor32Ptr: negative size argument not supported"
xor32Ptr !mask0 !p0 !n
  | assert (p0 <= p1 && p1 <= p2 && p2 <= p3 && n0 < 4 && n2 < 4) False = undefined -- assert invariants
  | n1 == 0 = xor32PtrNonAligned mask0 p0 n
  | n0 == 0 = do
      xor32PtrAligned    mask0 p1 n1
      xor32PtrNonAligned mask0 p2 n2
  | otherwise = do
      mask1 <- xor32PtrNonAligned mask0 p0 n0
      xor32PtrAligned    mask1 p1 n1
      xor32PtrNonAligned mask1 p2 n2
  where
    -- Invariants: p0 <= p1 <= p2 <= p3
    --             0  <= n0  < 4
    --             0  <= n1
    --             0  <= n2  < 4
    --             n  == n0+n1+n2 >= 4
    p1 = castPtr (alignPtr p0 d)
    p2 = alignPtrDown p3 d
    p3 = plusPtr p0 n
    d  = 4

    n0 = p1 `minusPtr` p0
    n1 = p2 `minusPtr` p1
    n2 = p3 `minusPtr` p2

-- internal
xor32PtrNonAligned :: Word32 -> Ptr Word8 -> Int -> IO Word32
xor32PtrNonAligned mask0 _ 0 = return mask0
xor32PtrNonAligned mask0 p 1 = do
  let mask1 = rotateL mask0 8
  xor8Ptr1 (fromIntegral mask1) p
  return mask1
xor32PtrNonAligned mask0 p 2 = do
  xor8Ptr1 (fromIntegral (mask0 `shiftR` 24)) p
  let mask1 = mask0 `rotateL` 16
  xor8Ptr1 (fromIntegral mask1) (p `plusPtr` 1)
  return mask1
xor32PtrNonAligned mask0 p 3 = do
  xor8Ptr1 (fromIntegral (mask0 `shiftR` 24)) p
  xor8Ptr1 (fromIntegral (mask0 `shiftR` 16)) (p `plusPtr` 1)
  let mask1 = mask0 `rotateL` 24
  xor8Ptr1 (fromIntegral mask1) (p `plusPtr` 2)
  return mask1
xor32PtrNonAligned mask0 p0 n = go mask0 p0
  where
    p' = p0 `plusPtr` n
    go m p
      | p == p'   = return m
      | otherwise = do
          let m' = rotateL m 8
          xor8Ptr1 (fromIntegral m') p
          go m' (p `plusPtr` 1)

-- internal
xor32PtrAligned :: Word32 -> Ptr Word32 -> Int -> IO ()
xor32PtrAligned _ _ 0 = return ()
xor32PtrAligned mask0be p0 n
  = assert (p0 `remPtr` 4 == 0 && n `rem` 4 == 0) $ go p0
  where
    p' = p0 `plusPtr` n
    go p
      | p == p'   = return ()
      | otherwise = do { xor32Ptr1 mask0 p; go (p `plusPtr` 4) }

    mask0 = case targetByteOrder of
              LittleEndian -> byteSwap32 mask0be
              BigEndian    -> mask0be

----------------------------------------------------------------------------

remPtr :: Ptr a -> Int -> Int
remPtr (X.Ptr x) (X.I# d) = X.I# (X.remAddr# x d)

alignPtrDown :: Ptr a -> Int -> Ptr a
alignPtrDown p i
  = case remPtr p i of
      0 -> p
      n -> plusPtr p (negate n)

xor8Ptr1 :: Word8 -> Ptr Word8 -> IO ()
xor8Ptr1 msk ptr  = do { x <- peek ptr; poke ptr (xor msk x) }

-- xor16Ptr1 :: Word16 -> Ptr Word16 -> IO ()
-- xor16Ptr1 msk ptr = do { x <- peek ptr; poke ptr (xor msk x) }

xor32Ptr1 :: Word32 -> Ptr Word32 -> IO ()
xor32Ptr1 msk ptr = do { x <- peek ptr; poke ptr (xor msk x) }

{-# INLINE unsafeCreate' #-}
unsafeCreate' :: Int -> (Ptr Word8 -> IO a) -> (BS.ByteString, a)
unsafeCreate' l0 f0 = unsafeDupablePerformIO (create' l0 f0)
  where
    {-# INLINE create' #-}
    create' :: Int -> (Ptr Word8 -> IO a) -> IO (BS.ByteString, a)
    create' l f = do
        fp <- mallocByteString l
        res <- withForeignPtr fp $ \p -> f p
        return (BS.PS fp 0 l, res)

----------------------------------------------------------------------------
-- single octet masks -- trivially mapped to 32-bit versions

expandW8ToW32 :: Word8 -> Word32
expandW8ToW32 x = x' .|. (x' `shiftL` 16)
  where
    x' = fromIntegral x .|. (fromIntegral x `shiftL` 8)


-- | Apply 8-bit XOR mask to each octet of a 'BS.ByteString'.
--
-- >>> xor8StrictByteString 0x20 "Hello"
-- "hELLO"
--
-- This function is a faster implementation of the semantically equivalent function shown below:
--
-- > xor8StrictByteString'ref :: Word8 -> BS.ByteString -> BS.ByteString
-- > xor8StrictByteString'ref 0    = id
-- > xor8StrictByteString'ref msk0 = BS.map (xor msk0)
--
xor8StrictByteString :: Word8 -> BS.ByteString -> BS.ByteString
xor8StrictByteString x = xor32StrictByteString (expandW8ToW32 x)

-- | Apply 8-bit XOR mask to each octet of a lazy 'BL.ByteString'.
--
-- See also 'xor8StrictByteString'
xor8LazyByteString :: Word8 -> BL.ByteString -> BL.ByteString
xor8LazyByteString x = xor32LazyByteString (expandW8ToW32 x)

-- | Apply 8-bit XOR mask to each octet of a 'SBS.ShortByteString'.
--
-- See also 'xor8StrictByteString'
xor8ShortByteString :: Word8 -> SBS.ShortByteString -> SBS.ShortByteString
xor8ShortByteString x = xor32ShortByteString (expandW8ToW32 x)

-- | Apply 8-bit XOR mask to each octet of a memory region expressed as start address and length in bytes.
--
-- See also 'xor8StrictByteString'
xor8CStringLen :: Word8 -> CStringLen -> IO ()
xor8CStringLen x (p,l) = void (xor32Ptr (expandW8ToW32 x) (castPtr p) l)

----------------------------------------------------------------------------
-- The missing mutable ShortByteString abstraction

data MShortByteString s = MSBS (X.MutableByteArray# s)

newSBS :: Int -> ST s (MShortByteString s)
newSBS (X.I# len#) = X.ST $ \s0 -> case X.newByteArray# len# s0 of (# s, mba# #) -> (# s, MSBS mba# #)

indexWord8Array :: ShortByteString -> Int -> Word8
indexWord8Array (SBS ba#) (X.I# i#) = X.W8# (X.indexWord8Array# ba# i#)

writeWord8Array :: MShortByteString s -> Int -> Word8 -> ST s ()
writeWord8Array (MSBS mba#) (X.I# i#) (X.W8# w#) = X.ST $ \s0 -> case X.writeWord8Array# mba# i# w# s0 of s -> (# s, () #)

indexWord32Array :: ShortByteString -> Int -> Word32
indexWord32Array (SBS ba#) (X.I# i#) = X.W32# (X.indexWord32Array# ba# i#)

writeWord32Array :: MShortByteString s -> Int -> Word32 -> ST s ()
writeWord32Array (MSBS mba#) (X.I# i#) (X.W32# w#) = X.ST $ \s0 -> case X.writeWord32Array# mba# i# w# s0 of s -> (# s, () #)

unsafeFreezeSBS :: MShortByteString s -> ST s ShortByteString
unsafeFreezeSBS (MSBS mba#) = X.ST $ \s0 -> case X.unsafeFreezeByteArray# mba# s0 of (# s, ba# #) -> (# s, SBS ba# #)
