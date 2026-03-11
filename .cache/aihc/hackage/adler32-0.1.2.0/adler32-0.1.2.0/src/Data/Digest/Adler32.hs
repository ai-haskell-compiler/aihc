{- |
Module      : Data.Digest.Adler32

Stability   : provisional
Portability : portable

An implementation of the Adler-32 checksum algorithm. There are two ways
to use this module:

* 'adler32' and 'adler32Update' which use 'Word32' for checksums,
* 'adler32'' and 'adler32Update'' which use the abstract type 'Adler32' for checksums.
This mode is slightly more low-level ('extractAdler32' has to be used to obtain
a 'Word32' for the checksum), but it supports some additional operations such
rolling checksum and compounding.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Data.Digest.Adler32
    ( Adler32Src(..)
    , Adler32
    , extractAdler32
    , makeAdler32
    , adler32SlideL
    , adler32SlideR
    , adler32AppendByte
    , adler32UnAppendByte
    , adler32PrependByte
    , adler32UnPrependByte
    , adler32UnAppend
    , adler32UnPrepend
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word32)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.|.), (.&.))
import Data.Semigroup (Semigroup(..))

#ifdef USE_ZLIB
import qualified Foreign as F
import qualified Foreign.C as F
import qualified System.IO.Unsafe as U
#endif

-- | Types of messages for which the Adler-32 checksum can be computed.
class Adler32Src a where
    -- | Compute the Adler-32 checksum of a @ByteString@.
    adler32 :: a -> Word32
    adler32 = extractAdler32 . adler32'

    -- | Update the checksum of a message by providing a @ByteString@ to be
    -- appended to the original message.
    adler32Update :: Word32 -> a -> Word32
    -- use a length of 0, since we will extract the checksum eventually
    -- and length of the first part is not used
    adler32Update c s =
        extractAdler32 $ makeAdler32 c (0 :: Word32) <> adler32' s

    -- | Similar to 'adler32' except that an 'Adler32' value is returned.
    adler32' :: a -> Adler32
    adler32' = adler32Update' mempty
    {-# INLINE adler32' #-}

    -- | Similar to 'adler32update' except that it operates on 'Adler32' values.
    -- An 'Adler32' value can also be updated with 'adler32'' in conjunction
    -- with the @Monoid@ instance of that type.
    adler32Update' :: Adler32 -> a -> Adler32

-- | An abstract representation of an Adler-32 checksum. Forcing a value of
-- this type to whnf will cause it to be evaluated completely.
data Adler32 =
    -- invariant: all 3 paramaters are <base
    Adler32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
  deriving (Eq, Ord)

instance Show Adler32 where
    show c@(Adler32 _ _ l) =
        "makeAdler32 " ++ show (extractAdler32 c) ++ " " ++ show l

-- | Extract the actual Adler-32 checksum from a 'Adler32' object.
extractAdler32 :: Adler32 -> Word32
extractAdler32 (Adler32 a b _) = a .|. (b `unsafeShiftL` 16)
{-# INLINE extractAdler32 #-}

-- | @makeAdler32 c l@ will create an 'Adler32' object that corresponds to
-- a message whose checksum is @c@ and length is @l@.
makeAdler32 :: Integral a => Word32 -> a -> Adler32
makeAdler32 c l =
    Adler32 (mod0 $ c .&. 0xffff) (mod0 $ c `unsafeShiftR` 16) (fromIntegral $ l `mod` base)
{-# INLINE makeAdler32 #-}

#ifdef USE_ZLIB
foreign import ccall unsafe "adler32"
    zlib_adler32 :: F.Word32 -> F.Ptr a -> F.CUInt -> F.Word32
#else
runAdler32 :: B.ByteString -> Word32 -> Word32 -> Word32 -> Adler32
runAdler32 s a0 b0 l0 = loop a0 b0 0 (min nmax len)
  where
    loop !a !b !i !j
        | i < j = loop a' (b + a') (i + 1) j
        | j < len = loop (mod1 a) (mod1 b) i (min (i + nmax) len)
        | otherwise = Adler32 (mod1 a) (mod1 b) (mod1 (l0 + fromIntegral len))
      where
        a' = a + fromIntegral (B.unsafeIndex s i)
    len = B.length s
    nmax = 5552
#endif

instance Adler32Src B.ByteString where
#ifndef USE_ZLIB
    adler32' s = runAdler32 s 1 0 0

    adler32Update' (Adler32 a b l) s =
        runAdler32 s a b l
#else
    adler32 = adler32Update 1
    {-# INLINE adler32 #-}

    adler32Update c s =
        U.unsafePerformIO $
            B.unsafeUseAsCStringLen s $ \(ptr, len) -> do
                return $ zlib_adler32 c ptr (fromIntegral len)
    {-# NOINLINE adler32Update #-}

    adler32' s = makeAdler32 (adler32 s) (B.length s)

    adler32Update' c@(Adler32 _ _ l) s =
        makeAdler32 (adler32Update (extractAdler32 c) s) (l + fromIntegral (B.length s))
#endif

instance Adler32Src BL.ByteString where
    adler32Update' = BL.foldlChunks (\c s -> c <> adler32' s)

instance Semigroup Adler32 where
    Adler32 a1 b1 l1 <> Adler32 a2 b2 l2 =
        Adler32 (mod0 $ a1m1 + a2) b (mod0 $ l1 + l2)
      where
        b = mod1 $ b1 + b2 + l2 * a1m1
        a1m1 = if a1 == 0 then base - 1 else a1 - 1

-- | 'mempty' is the checksum of the empty message and '<>' computes the
-- checksum of the concatenation of two messages. '<>' is an /O(1)/
-- operation.
instance Monoid Adler32 where
    mempty = Adler32 1 0 0

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

-- | /O(1)/. If @c@ is the checksum of a message that starts with the
-- byte @d1@ then @adler32SlideL d1 c d2@ is the checksum of the message
-- that is obtained by removing the first byte and appending the byte @d2@
-- at the end of the original message. It is the caller's responsibility to
-- ensure that the original message starts with the byte @d1@.
adler32SlideL :: Word8 -> Adler32 -> Word8 -> Adler32
adler32SlideL d1 c d2 =
    d1 `adler32UnPrependByte` (c `adler32AppendByte` d2)
{-# INLINE adler32SlideL #-}

-- | /O(1)/. Similar to 'adler32SlideL' except that it slides the checksum
-- window to the other direction, i.e. in @adler32SlideR d1 c d2@ the byte
-- @d2@ will be removed from the end of the original message and the byte
-- @d1@ will be prepended to its beginning. It is the caller's responsibility
-- to ensure that the original message ends with the byte @d2@.
adler32SlideR :: Word8 -> Adler32 -> Word8 -> Adler32
adler32SlideR d1 c d2 =
    (d1 `adler32PrependByte` c) `adler32UnAppendByte` d2
{-# INLINE adler32SlideR #-}

-- | /O(1)/. Given the checksum of a message, this function returns the
-- checksum of that message with a byte appended to it.
adler32AppendByte :: Adler32 -> Word8 -> Adler32
adler32AppendByte (Adler32 a b l) d =
    Adler32 a' (mod0 $ b + a') (mod0 $ l + 1)
  where
    a' = mod0 $ a + fromIntegral d

-- | /O(1)/. Given the checksum of a message, this function returns the
-- checksum of that message its last byte removed from it. The value of
-- that byte has to be provided by the caller and the behavior of the
-- function is unspecified if that value is incorrect.
adler32UnAppendByte :: Adler32 -> Word8 -> Adler32
adler32UnAppendByte (Adler32 a b l) d =
    Adler32 (modDiff a (fromIntegral d)) (modDiff b a) (modDiff l 1)

-- | /O(1)/. Given the checksum of a message, this function returns the
-- checksum of that message with a byte prepended to it.
adler32PrependByte :: Word8 -> Adler32 -> Adler32
adler32PrependByte d (Adler32 a b l) =
    Adler32 (mod0 $ a + fromIntegral d) (mod1 $ b + l' * fromIntegral d + 1) l'
  where
    l' = mod0 $ l + 1

-- | /O(1)/. Given the checksum of a message, this function returns the
-- checksum of that message its first byte removed from it. The value of
-- that byte has to be provided by the caller and the behavior of the
-- function is unspecified if that value is incorrect.
adler32UnPrependByte :: Word8 -> Adler32 -> Adler32
adler32UnPrependByte d (Adler32 a b l) =
    Adler32 (modDiff a (fromIntegral d)) (modDiff b (mod1 $ l * fromIntegral d + 1)) (modDiff l 1)

-- | /O(1)/. If @s1@ and @s2@ are two messages then @adler32UnAppend c c2@
-- returns the checksum of @s1@ where @c@ is the checksum of @s1 <> s2@ and
-- @c2@ is the checksum of @s2@.
adler32UnAppend :: Adler32 -> Adler32 -> Adler32
adler32UnAppend (Adler32 a b l) (Adler32 a2 b2 l2) =
    Adler32 a1 b1 (modDiff l l2)
  where
    b1 = modDiff b (mod1 $ b2 + l2 * a1m1)
    a1m1 = if a1 == 0 then base - 1 else a1 - 1
    a1 = mod0 $ modDiff a a2 + 1

-- | /O(1)/. If @s1@ and @s2@ are two messages then @adler32UnAppend c1 c@
-- returns the checksum of @s2@ where @c@ is the checksum of @s1 <> s2@ and
-- @c1@ is the checksum of @s1@.
adler32UnPrepend :: Adler32 -> Adler32 -> Adler32
adler32UnPrepend (Adler32 a1 b1 l1) (Adler32 a b l) =
    Adler32 (modDiff a a1m1) b2 l2
  where
    b2 = modDiff b (mod1 $ b1 + l2 * a1m1)
    a1m1 = if a1 == 0 then base - 1 else a1 - 1
    l2 = modDiff l l1

-- expects that 0 <= x < 2 * base
mod0 :: Integral a => a -> a
mod0 x
    | x < base = x
    | otherwise = x - base
{-# INLINE mod0 #-}

mod1 :: Integral a => a -> a
mod1 x = x `rem` base
{-# INLINE mod1 #-}

-- expects that 0 <= x < base, same for y
modDiff :: Integral a => a -> a -> a
modDiff x y
    | x >= y = x - y
    | otherwise = (x + base) - y
{-# INLINE modDiff #-}

base :: Num a => a
base = 65521
{-# INLINE base #-}
