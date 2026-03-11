{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- | The functions in this module are explict about the maximum number
of bytes they require.
-}
module Data.Bytes.Builder.Bounded
  ( -- * Builder
    Builder

    -- * Execute
  , run
  , runByteString
  , pasteGrowST

    -- * Combine
  , empty
  , append

    -- * Bounds Manipulation
  , weaken
  , substitute

    -- * Encode Integral Types

    -- ** Human-Readable
  , word64Dec
  , word32Dec
  , word16Dec
  , word8Dec
  , wordDec
  , int64Dec
  , int32Dec
  , int16Dec
  , int8Dec
  , intDec

    -- * Unsigned Words

    -- ** Wide Words
  , word128PaddedLowerHex
  , word128PaddedUpperHex
  , word256PaddedLowerHex
  , word256PaddedUpperHex

    -- ** 64-bit
  , word64PaddedLowerHex
  , word64PaddedUpperHex

    -- ** 48-bit
  , word48PaddedLowerHex

    -- ** 32-bit
  , word32PaddedLowerHex
  , word32PaddedUpperHex

    -- ** 16-bit
  , word16PaddedLowerHex
  , word16PaddedUpperHex
  , word16LowerHex
  , word16UpperHex

    -- ** 8-bit
  , word8PaddedLowerHex
  , word8PaddedUpperHex
  , word8LowerHex
  , ascii
  , ascii2
  , ascii3
  , ascii4
  , ascii5
  , ascii6
  , ascii7
  , ascii8
  , char

    -- ** Native
  , wordPaddedDec2
  , wordPaddedDec3
  , wordPaddedDec4
  , wordPaddedDec9

    -- ** Machine-Readable

    -- *** One
  , word8

    -- **** Big Endian
  , word256BE
  , word128BE
  , word64BE
  , word32BE
  , word16BE
  , int64BE
  , int32BE
  , int16BE

    -- **** Little Endian
  , word256LE
  , word128LE
  , word64LE
  , word32LE
  , word16LE
  , int64LE
  , int32LE
  , int16LE

    -- **** LEB128

    -- | LEB128 encodes an integer in 7-bit units, least significant bits first,
    -- with the high bit of each output byte set to 1 in all bytes except for
    -- the final byte.
  , wordLEB128
  , word16LEB128
  , word32LEB128
  , word64LEB128

    -- **** VLQ

    -- | VLQ (also known as VByte, Varint, VInt) encodes an integer in 7-bit
    -- units, most significant bits first, with the high bit of each output byte
    -- set to 1 in all bytes except for the final byte.
  , wordVlq
  , word32Vlq
  , word64Vlq

    -- * Encode Floating-Point Types
  , doubleDec
  ) where

import Arithmetic.Types (type (:=:), type (<=))
import Control.Monad.Primitive (primitive_)
import Control.Monad.ST (ST)
import Control.Monad.ST.Run (runByteArrayST, runIntByteArrayST)
import Data.Bits
import Data.ByteString (ByteString)
import Data.Bytes.Builder.Bounded.Unsafe (Builder (..))
import Data.Bytes.Types (Bytes (Bytes))
import Data.Char (ord)
import Data.Primitive (ByteArray, MutableByteArray (..), newByteArray, readByteArray, unsafeFreezeByteArray, writeByteArray)
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset (..))
import Data.WideWord (Word128 (Word128), Word256 (Word256))
import GHC.Exts
import GHC.IO (unsafeIOToST)
import GHC.Int (Int16 (I16#), Int32 (I32#), Int64 (I64#), Int8 (I8#))
import GHC.ST (ST (ST))
import GHC.TypeLits (type (+))
import GHC.Word (Word16 (W16#), Word32 (W32#), Word64 (W64#), Word8 (W8#))

import qualified Compat as C

import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Types as Arithmetic
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder.Bounded.Unsafe as Unsafe
import qualified Data.Primitive as PM

{- | Execute the bounded builder. If the size is a constant,
use @Arithmetic.Nat.constant@ as the first argument to let
GHC conjure up this value for you.
-}
run ::
  Arithmetic.Nat n ->
  -- | Builder
  Builder n ->
  ByteArray
{-# INLINE run #-}
run n b = runByteArrayST $ do
  arr <- newByteArray (Nat.demote n)
  len <- Unsafe.pasteST b arr 0
  shrinkMutableByteArray arr len
  unsafeFreezeByteArray arr

{- | Variant of 'run' that puts the result in a pinned buffer and
packs it up in a 'ByteString'.
-}
runByteString ::
  Arithmetic.Nat n ->
  -- | Builder
  Builder n ->
  ByteString
{-# INLINE runByteString #-}
runByteString n b =
  let (finalLen, r) = runIntByteArrayST $ do
        arr <- PM.newPinnedByteArray (Nat.demote n)
        len <- Unsafe.pasteST b arr 0
        shrinkMutableByteArray arr len
        arr' <- unsafeFreezeByteArray arr
        pure (len, arr')
   in Bytes.pinnedToByteString (Bytes r 0 finalLen)

{- | Paste the builder into the byte array starting at offset zero.
This reallocates the byte array if it cannot accomodate the builder,
growing it by the minimum amount necessary.
-}
pasteGrowST ::
  Arithmetic.Nat n ->
  Builder n ->
  -- | Initial buffer, used linearly. Do not reuse this argument.
  MutableByteArrayOffset s ->
  -- | Final buffer that accomodated the builder.
  ST s (MutableByteArrayOffset s)
{-# INLINE pasteGrowST #-}
pasteGrowST n b !(MutableByteArrayOffset {array = arr0, offset = off0}) = do
  sz0 <- PM.getSizeofMutableByteArray arr0
  let req = Nat.demote n
  let sz1 = off0 + req
  if sz1 <= sz0
    then do
      off1 <- Unsafe.pasteST b arr0 off0
      pure (MutableByteArrayOffset arr0 off1)
    else do
      arr1 <- PM.resizeMutableByteArray arr0 sz1
      off1 <- Unsafe.pasteST b arr1 off0
      pure (MutableByteArrayOffset arr1 off1)

-- | The monoidal unit of `append`
empty :: Builder 0
empty = Builder $ \_ off0 s0 -> (# s0, off0 #)

infixr 9 `append`

-- | Concatenate two builders.
append :: Builder m -> Builder n -> Builder (m + n)
append = unsafeAppend

unsafeAppend :: Builder m -> Builder n -> Builder p
unsafeAppend (Builder f) (Builder g) =
  Builder $ \arr off0 s0 -> case f arr off0 s0 of
    (# s1, r #) -> g arr r s1

{- | Weaken the bound on the maximum number of bytes required. For example,
to use two builders with unequal bounds in a disjunctive setting:

> import qualified Arithmetic.Lte as Lte
>
> buildNumber :: Either Double Word64 -> Builder 32
> buildNumber = \case
>   Left d  -> doubleDec d
>   Right w -> weaken (Lte.constant @19 @32) (word64Dec w)
-}
weaken :: forall m n. (m <= n) -> Builder m -> Builder n
weaken !_ (Builder f) = Builder f

-- | Replace the upper bound on size with an equal number.
substitute :: forall m n. (m :=: n) -> Builder m -> Builder n
substitute !_ (Builder f) = Builder f

{- | Encode a double-floating-point number, using decimal notation or
scientific notation depending on the magnitude. This has undefined
behavior when representing @+inf@, @-inf@, and @NaN@. It will not
crash, but the generated numbers will be nonsense.
-}
doubleDec :: Double -> Builder 32
doubleDec (D# d) = Builder (\arr off0 s0 -> doubleDec# d arr off0 s0)

{- | Requires up to 19 bytes. Encodes an unsigned 64-bit integer as decimal.
This encoding never starts with a zero unless the argument was zero.
-}
word64Dec :: Word64 -> Builder 19
word64Dec (W64# w) = wordCommonDec# (word64ToWord# w)

{- | Requires up to 10 bytes. Encodes an unsigned 32-bit integer as decimal.
This encoding never starts with a zero unless the argument was zero.
-}
word32Dec :: Word32 -> Builder 10
word32Dec (W32# w) = wordCommonDec# (C.word32ToWord# w)

{- | Requires up to 5 bytes. Encodes an unsigned 16-bit integer as decimal.
This encoding never starts with a zero unless the argument was zero.
-}
word16Dec :: Word16 -> Builder 5
word16Dec (W16# w) = wordCommonDec# (C.word16ToWord# w)

{- | Requires up to 3 bytes. Encodes an unsigned 8-bit integer as decimal.
This encoding never starts with a zero unless the argument was zero.
-}
word8Dec :: Word8 -> Builder 3
word8Dec (W8# w) =
  -- We unroll the loop when encoding Word8s. This speeds things
  -- up IPv4 encoding by about 10% in the @ip@ library. We can
  -- encode Word8s at twice this speed by using a lookup table.
  -- However, I (Andrew Martin) am concerned that although lookup
  -- table perform very well in microbenchmarks, they can thrash
  -- L1 cache in real applications.
  word8Dec# (C.word8ToWord# w)

{- | Requires up to 19 bytes. Encodes an unsigned machine-sized integer
as decimal. This encoding never starts with a zero unless the argument
was zero.
-}
wordDec :: Word -> Builder 19
wordDec (W# w) = wordCommonDec# w

{- | Requires up to 20 bytes. Encodes a signed 64-bit integer as decimal.
This encoding never starts with a zero unless the argument was zero.
Negative numbers are preceded by a minus sign. Positive numbers
are not preceded by anything.
-}
int64Dec :: Int64 -> Builder 20
int64Dec (I64# w) = intCommonDec# (int64ToInt# w)

{- | Requires up to 11 bytes. Encodes a signed 32-bit integer as decimal.
This encoding never starts with a zero unless the argument was zero.
Negative numbers are preceded by a minus sign. Positive numbers
are not preceded by anything.
-}
int32Dec :: Int32 -> Builder 11
int32Dec (I32# w) = intCommonDec# (C.int32ToInt# w)

{- | Requires up to 6 bytes. Encodes a signed 16-bit integer as decimal.
This encoding never starts with a zero unless the argument was zero.
Negative numbers are preceded by a minus sign. Positive numbers
are not preceded by anything.
-}
int16Dec :: Int16 -> Builder 6
int16Dec (I16# w) = intCommonDec# (C.int16ToInt# w)

{- | Requires up to 4 bytes. Encodes a signed 8-bit integer as decimal.
This encoding never starts with a zero unless the argument was zero.
Negative numbers are preceded by a minus sign. Positive numbers
are not preceded by anything.
-}
int8Dec :: Int8 -> Builder 4
int8Dec (I8# w) = intCommonDec# (C.int8ToInt# w)

{- | Requires up to 20 bytes. Encodes a signed machine-sized integer
as decimal. This encoding never starts with a zero unless the
argument was zero. Negative numbers are preceded by a minus sign.
Positive numbers are not preceded by anything.
-}
intDec :: Int -> Builder 20
intDec (I# w) = intCommonDec# w

word8Dec# :: Word# -> Builder 3
{-# NOINLINE word8Dec# #-}
word8Dec# w# = Unsafe.construct $ \arr off0 -> do
  let !(I# off0#) = off0
      !(!x, !ones) = quotRem w 10
      !(hundreds@(W# hundreds#), tens@(W# tens#)) = quotRem x 10
  writeByteArray arr off0 (fromIntegral (hundreds + 0x30) :: Word8)
  let !hasHundreds = gtWord# hundreds# 0##
      !off1@(I# off1#) = I# (off0# +# hasHundreds)
  writeByteArray arr off1 (fromIntegral (tens + 0x30) :: Word8)
  let !off2 = I# (off1# +# (orI# hasHundreds (gtWord# tens# 0##)))
  writeByteArray arr off2 (fromIntegral (ones + 0x30) :: Word8)
  pure (off2 + 1)
 where
  w = W# w#

-- Requires a number of bytes that is bounded by the size of
-- the word. This is only used internally.
wordCommonDec# :: Word# -> Builder n
{-# NOINLINE wordCommonDec# #-}
wordCommonDec# w# = Unsafe.construct $ \arr off0 ->
  if w /= 0
    then internalWordLoop arr off0 (W# w#)
    else do
      writeByteArray arr off0 (c2w '0')
      pure (off0 + 1)
 where
  w = W64# (wordToWord64# w#)

internalWordLoop :: MutableByteArray s -> Int -> Word -> ST s Int
{-# INLINE internalWordLoop #-}
internalWordLoop arr off0 x0 = do
  off1 <- backwardsWordLoop arr off0 x0
  reverseBytes arr off0 (off1 - 1)
  pure off1

backwardsWordLoop :: MutableByteArray s -> Int -> Word -> ST s Int
{-# INLINE backwardsWordLoop #-}
backwardsWordLoop arr off0 x0 = go off0 x0
 where
  go !off !(x :: Word) =
    if x > 0
      then do
        let (y, z) = quotRem x 10
        writeByteArray arr off (fromIntegral (z + 0x30) :: Word8)
        go (off + 1) y
      else pure off

-- Requires up to 20 bytes. Can be less depending on what the
-- size of the argument is known to be. Unsafe.
intCommonDec# :: Int# -> Builder n
{-# NOINLINE intCommonDec# #-}
intCommonDec# w# = Unsafe.construct $ \arr off0 -> case compare w 0 of
  GT -> internalWordLoop arr off0 (fromIntegral w)
  EQ -> do
    writeByteArray arr off0 (c2w '0')
    pure (off0 + 1)
  LT -> do
    writeByteArray arr off0 (c2w '-')
    internalWordLoop arr (off0 + 1) (fromIntegral (negate w))
 where
  w = I64# (intToInt64# w#)

-- Convert a number between 0 and 16 to the ASCII
-- representation of its hexadecimal character.
-- The use of fromIntegral causes us to incur an
-- unneeded bitmask. This actually needs a Word64
-- argument.
toHexUpper :: Word -> Word8
toHexUpper w' =
  fromIntegral $
    (complement theMask .&. loSolved)
      .|. (theMask .&. hiSolved)
 where
  w = w' .&. 0xF
  -- This is all ones if the value was >= 10
  theMask = (1 .&. unsafeShiftR (w - 10) 63) - 1
  loSolved = w + 48
  hiSolved = w + 55

toHexLower :: Word -> Word8
toHexLower w' =
  fromIntegral $
    (complement theMask .&. loSolved)
      .|. (theMask .&. hiSolved)
 where
  w = w' .&. 0xF
  -- This is all ones if the value was >= 10
  theMask = (1 .&. unsafeShiftR (w - 10) 63) - 1
  loSolved = w + 48
  hiSolved = w + 87

{- | Requires exactly 64 bytes. Encodes a 256-bit unsigned integer as
hexadecimal, zero-padding the encoding to 64 digits. This uses
lowercase for the alphabetical digits.
-}
word256PaddedLowerHex :: Word256 -> Builder 64
word256PaddedLowerHex (Word256 w192 w128 w64 w0) =
  word64PaddedLowerHex w192
    `append` word64PaddedLowerHex w128
    `append` word64PaddedLowerHex w64
    `append` word64PaddedLowerHex w0

{- | Requires exactly 64 bytes. Encodes a 256-bit unsigned integer as
hexadecimal, zero-padding the encoding to 64 digits. This uses
uppercase for the alphabetical digits.
-}
word256PaddedUpperHex :: Word256 -> Builder 64
word256PaddedUpperHex (Word256 w192 w128 w64 w0) =
  word64PaddedUpperHex w192
    `append` word64PaddedUpperHex w128
    `append` word64PaddedUpperHex w64
    `append` word64PaddedUpperHex w0

{- | Requires exactly 32 bytes. Encodes a 128-bit unsigned integer as
hexadecimal, zero-padding the encoding to 32 digits. This uses
lowercase for the alphabetical digits.
-}
word128PaddedLowerHex :: Word128 -> Builder 32
word128PaddedLowerHex (Word128 w64 w0) =
  word64PaddedLowerHex w64
    `append` word64PaddedLowerHex w0

{- | Requires exactly 32 bytes. Encodes a 128-bit unsigned integer as
hexadecimal, zero-padding the encoding to 32 digits. This uses
uppercase for the alphabetical digits.
-}
word128PaddedUpperHex :: Word128 -> Builder 32
word128PaddedUpperHex (Word128 w64 w0) =
  word64PaddedUpperHex w64
    `append` word64PaddedUpperHex w0

{- | Requires exactly 16 bytes. Encodes a 64-bit unsigned integer as
hexadecimal, zero-padding the encoding to 16 digits. This uses
uppercase for the alphabetical digits. For example, this encodes the
number 1022 as @00000000000003FE@.
-}
word64PaddedUpperHex :: Word64 -> Builder 16
word64PaddedUpperHex (W64# w) = word64PaddedUpperHex# (word64ToWord# w)

{- | Requires exactly 16 bytes. Encodes a 64-bit unsigned integer as
hexadecimal, zero-padding the encoding to 16 digits. This uses
lowercase for the alphabetical digits. For example, this encodes the
number 1022 as @00000000000003fe@.
-}
word64PaddedLowerHex :: Word64 -> Builder 16
word64PaddedLowerHex (W64# w) = word64PaddedLowerHex# (word64ToWord# w)

{- | Requires exactly 12 bytes. Discards the upper 16 bits of a
64-bit unsigned integer and then encodes the lower 48 bits as
hexadecimal, zero-padding the encoding to 12 digits. This uses
lowercase for the alphabetical digits. For example, this encodes the
number 1022 as @0000000003fe@.
-}
word48PaddedLowerHex :: Word64 -> Builder 12
word48PaddedLowerHex (W64# w) = word48PaddedLowerHex# (word64ToWord# w)

{- | Requires exactly 8 bytes. Encodes a 32-bit unsigned integer as
hexadecimal, zero-padding the encoding to 8 digits. This uses
uppercase for the alphabetical digits.
-}
word32PaddedUpperHex :: Word32 -> Builder 8
word32PaddedUpperHex (W32# w) = word32PaddedUpperHex# (C.word32ToWord# w)

{- | Requires exactly 8 bytes. Encodes a 32-bit unsigned integer as
hexadecimal, zero-padding the encoding to 8 digits. This uses
lowercase for the alphabetical digits.
-}
word32PaddedLowerHex :: Word32 -> Builder 8
word32PaddedLowerHex (W32# w) = word32PaddedLowerHex# (C.word32ToWord# w)

{- | Requires exactly 4 bytes. Encodes a 16-bit unsigned integer as
hexadecimal, zero-padding the encoding to 4 digits. This uses
uppercase for the alphabetical digits.

>>> word16PaddedUpperHex 0xab0
0AB0
-}
word16PaddedUpperHex :: Word16 -> Builder 4
word16PaddedUpperHex (W16# w) = word16PaddedUpperHex# (C.word16ToWord# w)

{- | Requires exactly 4 bytes. Encodes a 16-bit unsigned integer as
hexadecimal, zero-padding the encoding to 4 digits. This uses
lowercase for the alphabetical digits.

>>> word16PaddedLowerHex 0xab0
0ab0
-}
word16PaddedLowerHex :: Word16 -> Builder 4
word16PaddedLowerHex (W16# w) = word16PaddedLowerHex# (C.word16ToWord# w)

{- | Requires at most 4 bytes. Encodes a 16-bit unsigned integer as
hexadecimal. No leading zeroes are displayed. Letters are presented
in lowercase. If the number is zero, a single zero digit is used.

>>> word16LowerHex 0xab0
ab0
-}
word16LowerHex :: Word16 -> Builder 4
word16LowerHex (W16# w) = word16LowerHex# (C.word16ToWord# w)

{- | Requires at most 4 bytes. Encodes a 16-bit unsigned integer as
hexadecimal. No leading zeroes are displayed. Letters are presented
in uppercase. If the number is zero, a single zero digit is used.

>>> word16UpperHex 0xab0
AB0
-}
word16UpperHex :: Word16 -> Builder 4
word16UpperHex (W16# w) = word16UpperHex# (C.word16ToWord# w)

{- | Requires at most 2 bytes. Encodes a 8-bit unsigned integer as
hexadecimal. No leading zeroes are displayed. If the number is zero,
a single zero digit is used.
-}
word8LowerHex :: Word8 -> Builder 2
word8LowerHex (W8# w) = word8LowerHex# (C.word8ToWord# w)

{- | Requires exactly 2 bytes. Encodes a 8-bit unsigned integer as
hexadecimal, zero-padding the encoding to 2 digits. This uses
uppercase for the alphabetical digits.
-}
word8PaddedUpperHex :: Word8 -> Builder 2
word8PaddedUpperHex (W8# w) = word8PaddedUpperHex# (C.word8ToWord# w)

{- | Requires exactly 2 bytes. Encodes a 8-bit unsigned integer as
hexadecimal, zero-padding the encoding to 2 digits. This uses
lowercase for the alphabetical digits.
-}
word8PaddedLowerHex :: Word8 -> Builder 2
word8PaddedLowerHex (W8# w) = word8PaddedLowerHex# (C.word8ToWord# w)

-- TODO: Is it actually worth unrolling this loop. I suspect that it
-- might not be. Benchmark this.
word64PaddedUpperHex# :: Word# -> Builder 16
{-# NOINLINE word64PaddedUpperHex# #-}
word64PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 60))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 56))
  writeByteArray arr (off + 2) (toHexUpper (unsafeShiftR w 52))
  writeByteArray arr (off + 3) (toHexUpper (unsafeShiftR w 48))
  writeByteArray arr (off + 4) (toHexUpper (unsafeShiftR w 44))
  writeByteArray arr (off + 5) (toHexUpper (unsafeShiftR w 40))
  writeByteArray arr (off + 6) (toHexUpper (unsafeShiftR w 36))
  writeByteArray arr (off + 7) (toHexUpper (unsafeShiftR w 32))
  writeByteArray arr (off + 8) (toHexUpper (unsafeShiftR w 28))
  writeByteArray arr (off + 9) (toHexUpper (unsafeShiftR w 24))
  writeByteArray arr (off + 10) (toHexUpper (unsafeShiftR w 20))
  writeByteArray arr (off + 11) (toHexUpper (unsafeShiftR w 16))
  writeByteArray arr (off + 12) (toHexUpper (unsafeShiftR w 12))
  writeByteArray arr (off + 13) (toHexUpper (unsafeShiftR w 8))
  writeByteArray arr (off + 14) (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 15) (toHexUpper (unsafeShiftR w 0))
  pure (off + 16)
 where
  w = W# w#

-- TODO: Is it actually worth unrolling this loop. I suspect that it
-- might not be. Benchmark this.
word48PaddedLowerHex# :: Word# -> Builder 12
{-# NOINLINE word48PaddedLowerHex# #-}
word48PaddedLowerHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexLower (unsafeShiftR w 44))
  writeByteArray arr (off + 1) (toHexLower (unsafeShiftR w 40))
  writeByteArray arr (off + 2) (toHexLower (unsafeShiftR w 36))
  writeByteArray arr (off + 3) (toHexLower (unsafeShiftR w 32))
  writeByteArray arr (off + 4) (toHexLower (unsafeShiftR w 28))
  writeByteArray arr (off + 5) (toHexLower (unsafeShiftR w 24))
  writeByteArray arr (off + 6) (toHexLower (unsafeShiftR w 20))
  writeByteArray arr (off + 7) (toHexLower (unsafeShiftR w 16))
  writeByteArray arr (off + 8) (toHexLower (unsafeShiftR w 12))
  writeByteArray arr (off + 9) (toHexLower (unsafeShiftR w 8))
  writeByteArray arr (off + 10) (toHexLower (unsafeShiftR w 4))
  writeByteArray arr (off + 11) (toHexLower w)
  pure (off + 12)
 where
  w = W# w#

-- TODO: Is it actually worth unrolling this loop. I suspect that it
-- might not be. Benchmark this.
word64PaddedLowerHex# :: Word# -> Builder 16
{-# NOINLINE word64PaddedLowerHex# #-}
word64PaddedLowerHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexLower (unsafeShiftR w 60))
  writeByteArray arr (off + 1) (toHexLower (unsafeShiftR w 56))
  writeByteArray arr (off + 2) (toHexLower (unsafeShiftR w 52))
  writeByteArray arr (off + 3) (toHexLower (unsafeShiftR w 48))
  writeByteArray arr (off + 4) (toHexLower (unsafeShiftR w 44))
  writeByteArray arr (off + 5) (toHexLower (unsafeShiftR w 40))
  writeByteArray arr (off + 6) (toHexLower (unsafeShiftR w 36))
  writeByteArray arr (off + 7) (toHexLower (unsafeShiftR w 32))
  writeByteArray arr (off + 8) (toHexLower (unsafeShiftR w 28))
  writeByteArray arr (off + 9) (toHexLower (unsafeShiftR w 24))
  writeByteArray arr (off + 10) (toHexLower (unsafeShiftR w 20))
  writeByteArray arr (off + 11) (toHexLower (unsafeShiftR w 16))
  writeByteArray arr (off + 12) (toHexLower (unsafeShiftR w 12))
  writeByteArray arr (off + 13) (toHexLower (unsafeShiftR w 8))
  writeByteArray arr (off + 14) (toHexLower (unsafeShiftR w 4))
  writeByteArray arr (off + 15) (toHexLower (unsafeShiftR w 0))
  pure (off + 16)
 where
  w = W# w#

word32PaddedUpperHex# :: Word# -> Builder 8
{-# NOINLINE word32PaddedUpperHex# #-}
word32PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 28))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 24))
  writeByteArray arr (off + 2) (toHexUpper (unsafeShiftR w 20))
  writeByteArray arr (off + 3) (toHexUpper (unsafeShiftR w 16))
  writeByteArray arr (off + 4) (toHexUpper (unsafeShiftR w 12))
  writeByteArray arr (off + 5) (toHexUpper (unsafeShiftR w 8))
  writeByteArray arr (off + 6) (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 7) (toHexUpper (unsafeShiftR w 0))
  pure (off + 8)
 where
  w = W# w#

word32PaddedLowerHex# :: Word# -> Builder 8
{-# NOINLINE word32PaddedLowerHex# #-}
word32PaddedLowerHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexLower (unsafeShiftR w 28))
  writeByteArray arr (off + 1) (toHexLower (unsafeShiftR w 24))
  writeByteArray arr (off + 2) (toHexLower (unsafeShiftR w 20))
  writeByteArray arr (off + 3) (toHexLower (unsafeShiftR w 16))
  writeByteArray arr (off + 4) (toHexLower (unsafeShiftR w 12))
  writeByteArray arr (off + 5) (toHexLower (unsafeShiftR w 8))
  writeByteArray arr (off + 6) (toHexLower (unsafeShiftR w 4))
  writeByteArray arr (off + 7) (toHexLower (unsafeShiftR w 0))
  pure (off + 8)
 where
  w = W# w#

-- Not sure if it is beneficial to inline this. We just let
-- GHC make the decision. Open an issue on github if this is
-- a problem.
word16PaddedUpperHex# :: Word# -> Builder 4
word16PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 12))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 8))
  writeByteArray arr (off + 2) (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 3) (toHexUpper (unsafeShiftR w 0))
  pure (off + 4)
 where
  w = W# w#

word16PaddedLowerHex# :: Word# -> Builder 4
word16PaddedLowerHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexLower (unsafeShiftR w 12))
  writeByteArray arr (off + 1) (toHexLower (unsafeShiftR w 8))
  writeByteArray arr (off + 2) (toHexLower (unsafeShiftR w 4))
  writeByteArray arr (off + 3) (toHexLower (unsafeShiftR w 0))
  pure (off + 4)
 where
  w = W# w#

word12PaddedLowerHex# :: Word# -> Builder 3
word12PaddedLowerHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexLower (unsafeShiftR w 8))
  writeByteArray arr (off + 1) (toHexLower (unsafeShiftR w 4))
  writeByteArray arr (off + 2) (toHexLower (unsafeShiftR w 0))
  pure (off + 3)
 where
  w = W# w#

word12PaddedUpperHex# :: Word# -> Builder 3
word12PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 8))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 2) (toHexUpper (unsafeShiftR w 0))
  pure (off + 3)
 where
  w = W# w#

-- Definitely want this to inline. It's maybe a dozen instructions total.
word8PaddedUpperHex# :: Word# -> Builder 2
{-# INLINE word8PaddedUpperHex# #-}
word8PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 0))
  pure (off + 2)
 where
  w = W# w#

word8PaddedLowerHex# :: Word# -> Builder 2
{-# INLINE word8PaddedLowerHex# #-}
word8PaddedLowerHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexLower (unsafeShiftR w 4))
  writeByteArray arr (off + 1) (toHexLower (unsafeShiftR w 0))
  pure (off + 2)
 where
  w = W# w#

word4PaddedLowerHex# :: Word# -> Builder 1
{-# INLINE word4PaddedLowerHex# #-}
word4PaddedLowerHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexLower w)
  pure (off + 1)
 where
  w = W# w#

word4PaddedUpperHex# :: Word# -> Builder 1
{-# INLINE word4PaddedUpperHex# #-}
word4PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexUpper w)
  pure (off + 1)
 where
  w = W# w#

word16UpperHex# :: Word# -> Builder 4
word16UpperHex# w#
  | w <= 0xF = weaken Lte.constant (word4PaddedUpperHex# w#)
  | w <= 0xFF = weaken Lte.constant (word8PaddedUpperHex# w#)
  | w <= 0xFFF = weaken Lte.constant (word12PaddedUpperHex# w#)
  | otherwise = word16PaddedUpperHex# w#
 where
  w = W# w#

word16LowerHex# :: Word# -> Builder 4
word16LowerHex# w#
  | w <= 0xF = weaken Lte.constant (word4PaddedLowerHex# w#)
  | w <= 0xFF = weaken Lte.constant (word8PaddedLowerHex# w#)
  | w <= 0xFFF = weaken Lte.constant (word12PaddedLowerHex# w#)
  | otherwise = word16PaddedLowerHex# w#
 where
  w = W# w#

-- Precondition: argument less than 256
word8LowerHex# :: Word# -> Builder 2
word8LowerHex# w#
  | w <= 0xF = weaken Lte.constant (word4PaddedLowerHex# w#)
  | otherwise = weaken Lte.constant (word8PaddedLowerHex# w#)
 where
  w = W# w#

{- | Encode a number less than 100 as a decimal number, zero-padding it to
two digits. For example: 0 is encoded as @00@, 5 is encoded as @05@, and
73 is encoded as @73@.

Precondition: Argument must be less than 100. Failure to satisfy this
precondition will not result in a segfault, but the resulting bytes are
undefined. The implemention uses a heuristic for division that is inaccurate
for large numbers.
-}
wordPaddedDec2 :: Word -> Builder 2
wordPaddedDec2 !w = Unsafe.construct $ \arr off -> do
  let d1 = approxDiv10 w
      d2 = w - (10 * d1)
  writeByteArray arr off (unsafeWordToWord8 (d1 + 48))
  writeByteArray arr (off + 1) (unsafeWordToWord8 (d2 + 48))
  pure (off + 2)

{- | Encode a number less than 10000 as a decimal number, zero-padding it to
two digits. For example: 0 is encoded as @0000@, 5 is encoded as @0005@,
and 73 is encoded as @0073@.

Precondition: Argument must be less than 10000. Failure to satisfy this
precondition will not result in a segfault, but the resulting bytes are
undefined. The implemention uses a heuristic for division that is inaccurate
for large numbers.
-}
wordPaddedDec4 :: Word -> Builder 4
wordPaddedDec4 !w = Unsafe.construct $ \arr off -> do
  putRem10
    ( putRem10 $
        putRem10 $
          putRem10
            (\_ _ _ -> pure ())
    )
    arr
    (off + 3)
    w
  pure (off + 4)

wordPaddedDec3 :: Word -> Builder 3
wordPaddedDec3 !w = Unsafe.construct $ \arr off -> do
  putRem10
    ( putRem10 $
        putRem10
          (\_ _ _ -> pure ())
    )
    arr
    (off + 2)
    w
  pure (off + 3)

{- | Encode a number less than 1e9 as a decimal number, zero-padding it to
nine digits. For example: 0 is encoded as @000000000@ and 5 is encoded as
@000000005@.

Precondition: Argument must be less than 1e9. Failure to satisfy this
precondition will not result in a segfault, but the resulting bytes are
undefined. The implemention uses a heuristic for division that is inaccurate
for large numbers.
-}
wordPaddedDec9 :: Word -> Builder 9
wordPaddedDec9 !w = Unsafe.construct $ \arr off -> do
  putRem10
    ( putRem10 $
        putRem10 $
          putRem10 $
            putRem10 $
              putRem10 $
                putRem10 $
                  putRem10 $
                    putRem10
                      (\_ _ _ -> pure ())
    )
    arr
    (off + 8)
    w
  pure (off + 9)

putRem10 :: (MutableByteArray s -> Int -> Word -> ST s a) -> MutableByteArray s -> Int -> Word -> ST s a
{-# INLINE putRem10 #-}
putRem10 andThen arr off dividend = do
  let quotient = approxDiv10 dividend
      remainder = dividend - (10 * quotient)
  writeByteArray arr off (unsafeWordToWord8 (remainder + 48))
  andThen arr (off - 1) quotient

{- | Encode an ASCII character.
Precondition: Input must be an ASCII character. This is not checked.
-}
ascii :: Char -> Builder 1
ascii (C# c) = Unsafe.construct $ \(MutableByteArray arr) (I# off) -> do
  primitive_ (writeCharArray# arr off c)
  pure (I# (off +# 1#))

{- | Encode two ASCII characters. Precondition: Must be an ASCII characters.
This is not checked.
-}
ascii2 :: Char -> Char -> Builder 2
ascii2 (C# c0) (C# c1) = Unsafe.construct $ \(MutableByteArray arr) (I# off) -> do
  primitive_ (writeCharArray# arr off c0)
  primitive_ (writeCharArray# arr (off +# 1#) c1)
  pure (I# (off +# 2#))

{- | Encode three ASCII characters. Precondition: Must be an ASCII characters.
This is not checked.
-}
ascii3 :: Char -> Char -> Char -> Builder 3
ascii3 (C# c0) (C# c1) (C# c2) = Unsafe.construct $ \(MutableByteArray arr) (I# off) -> do
  primitive_ (writeCharArray# arr off c0)
  primitive_ (writeCharArray# arr (off +# 1#) c1)
  primitive_ (writeCharArray# arr (off +# 2#) c2)
  pure (I# (off +# 3#))

{- | Encode four ASCII characters. Precondition: Must be an ASCII characters.
This is not checked.
-}
ascii4 :: Char -> Char -> Char -> Char -> Builder 4
ascii4 (C# c0) (C# c1) (C# c2) (C# c3) = Unsafe.construct $ \(MutableByteArray arr) (I# off) -> do
  primitive_ (writeCharArray# arr off c0)
  primitive_ (writeCharArray# arr (off +# 1#) c1)
  primitive_ (writeCharArray# arr (off +# 2#) c2)
  primitive_ (writeCharArray# arr (off +# 3#) c3)
  pure (I# (off +# 4#))

{- | Encode five ASCII characters. Precondition: Must be an ASCII characters.
This is not checked.
-}
ascii5 :: Char -> Char -> Char -> Char -> Char -> Builder 5
ascii5 (C# c0) (C# c1) (C# c2) (C# c3) (C# c4) = Unsafe.construct $ \(MutableByteArray arr) (I# off) -> do
  primitive_ (writeCharArray# arr off c0)
  primitive_ (writeCharArray# arr (off +# 1#) c1)
  primitive_ (writeCharArray# arr (off +# 2#) c2)
  primitive_ (writeCharArray# arr (off +# 3#) c3)
  primitive_ (writeCharArray# arr (off +# 4#) c4)
  pure (I# (off +# 5#))

{- | Encode six ASCII characters. Precondition: Must be an ASCII characters.
This is not checked.
-}
ascii6 :: Char -> Char -> Char -> Char -> Char -> Char -> Builder 6
ascii6 (C# c0) (C# c1) (C# c2) (C# c3) (C# c4) (C# c5) = Unsafe.construct $ \(MutableByteArray arr) (I# off) -> do
  primitive_ (writeCharArray# arr off c0)
  primitive_ (writeCharArray# arr (off +# 1#) c1)
  primitive_ (writeCharArray# arr (off +# 2#) c2)
  primitive_ (writeCharArray# arr (off +# 3#) c3)
  primitive_ (writeCharArray# arr (off +# 4#) c4)
  primitive_ (writeCharArray# arr (off +# 5#) c5)
  pure (I# (off +# 6#))

{- | Encode seven ASCII characters. Precondition: Must be an ASCII characters.
This is not checked.
-}
ascii7 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Builder 7
ascii7 (C# c0) (C# c1) (C# c2) (C# c3) (C# c4) (C# c5) (C# c6) = Unsafe.construct $ \(MutableByteArray arr) (I# off) -> do
  primitive_ (writeCharArray# arr off c0)
  primitive_ (writeCharArray# arr (off +# 1#) c1)
  primitive_ (writeCharArray# arr (off +# 2#) c2)
  primitive_ (writeCharArray# arr (off +# 3#) c3)
  primitive_ (writeCharArray# arr (off +# 4#) c4)
  primitive_ (writeCharArray# arr (off +# 5#) c5)
  primitive_ (writeCharArray# arr (off +# 6#) c6)
  pure (I# (off +# 7#))

{- | Encode eight ASCII characters. Precondition: Must be an ASCII characters.
This is not checked.
-}
ascii8 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Builder 8
ascii8 (C# c0) (C# c1) (C# c2) (C# c3) (C# c4) (C# c5) (C# c6) (C# c7) = Unsafe.construct $ \(MutableByteArray arr) (I# off) -> do
  primitive_ (writeCharArray# arr off c0)
  primitive_ (writeCharArray# arr (off +# 1#) c1)
  primitive_ (writeCharArray# arr (off +# 2#) c2)
  primitive_ (writeCharArray# arr (off +# 3#) c3)
  primitive_ (writeCharArray# arr (off +# 4#) c4)
  primitive_ (writeCharArray# arr (off +# 5#) c5)
  primitive_ (writeCharArray# arr (off +# 6#) c6)
  primitive_ (writeCharArray# arr (off +# 7#) c7)
  pure (I# (off +# 8#))

-- | Encode a machine-sized word with VLQ (also known as VByte, Varint, VInt).
wordVlq :: Word -> Builder 10
{-# INLINE wordVlq #-}
wordVlq (W# w) = vlqCommon (W# w)

-- | Encode a 32-bit word with VLQ (also known as VByte, Varint, VInt).
word32Vlq :: Word32 -> Builder 5
{-# INLINE word32Vlq #-}
word32Vlq (W32# w) = vlqCommon (W# (C.word32ToWord# w))

-- | Encode a 64-bit word with VLQ (also known as VByte, Varint, VInt).
word64Vlq :: Word64 -> Builder 10
{-# INLINE word64Vlq #-}
word64Vlq (W64# w) = vlqCommon (W# (word64ToWord# w))

-- | Encode a machine-sized word with LEB-128.
wordLEB128 :: Word -> Builder 10
{-# INLINE wordLEB128 #-}
wordLEB128 (W# w) = lebCommon (W# w)

-- | Encode a 32-bit word with LEB-128.
word16LEB128 :: Word16 -> Builder 3
{-# INLINE word16LEB128 #-}
word16LEB128 (W16# w) = lebCommon (W# (C.word16ToWord# w))

-- | Encode a 32-bit word with LEB-128.
word32LEB128 :: Word32 -> Builder 5
{-# INLINE word32LEB128 #-}
word32LEB128 (W32# w) = lebCommon (W# (C.word32ToWord# w))

-- | Encode a 64-bit word with LEB-128.
word64LEB128 :: Word64 -> Builder 10
{-# INLINE word64LEB128 #-}
word64LEB128 (W64# w) = lebCommon (W# (word64ToWord# w))

vlqCommon :: Word -> Builder n
vlqCommon !w = case w of
  0 -> unsafeWord8 0
  _ ->
    let !startIx = 7 * quot (63 - countLeadingZeros w) 7
     in vlqStep startIx w

vlqStep ::
  Int -> -- start index, must be in range [0,63] and 7 must divide it evenly
  Word ->
  Builder n
vlqStep !ix !w
  | ix <= 0 =
      unsafeWord8 (unsafeWordToWord8 (unsafeShiftR w ix .&. 0b0111_1111))
  | otherwise =
      unsafeAppend
        (unsafeWord8 (unsafeWordToWord8 (unsafeShiftR w ix .|. 0b1000_0000)))
        (vlqStep (ix - 7) w)

lebCommon :: Word -> Builder n
lebCommon !w = case quotRem w 128 of
  (q, r) -> case q of
    0 -> unsafeWord8 (unsafeWordToWord8 r)
    _ ->
      unsafeAppend
        (unsafeWord8 (unsafeWordToWord8 (r .|. 0x80)))
        (lebCommon q)

-- | Encode a character as UTF-8. This only uses as much space as is required.
char :: Char -> Builder 4
char c
  | codepoint < 0x80 = Unsafe.construct $ \arr off -> do
      writeByteArray arr off (unsafeWordToWord8 codepoint)
      pure (off + 1)
  | codepoint < 0x800 = Unsafe.construct $ \arr off -> do
      writeByteArray arr off (unsafeWordToWord8 (byteTwoOne codepoint))
      writeByteArray arr (off + 1) (unsafeWordToWord8 (byteTwoTwo codepoint))
      return (off + 2)
  | codepoint >= 0xD800 && codepoint < 0xE000 = Unsafe.construct $ \arr off -> do
      -- Codepoint U+FFFD
      writeByteArray arr off (0xEF :: Word8)
      writeByteArray arr (off + 1) (0xBF :: Word8)
      writeByteArray arr (off + 2) (0xBD :: Word8)
      return (off + 3)
  | codepoint < 0x10000 = Unsafe.construct $ \arr off -> do
      writeByteArray arr off (unsafeWordToWord8 (byteThreeOne codepoint))
      writeByteArray arr (off + 1) (unsafeWordToWord8 (byteThreeTwo codepoint))
      writeByteArray arr (off + 2) (unsafeWordToWord8 (byteThreeThree codepoint))
      return (off + 3)
  | otherwise = Unsafe.construct $ \arr off -> do
      writeByteArray arr off (unsafeWordToWord8 (byteFourOne codepoint))
      writeByteArray arr (off + 1) (unsafeWordToWord8 (byteFourTwo codepoint))
      writeByteArray arr (off + 2) (unsafeWordToWord8 (byteFourThree codepoint))
      writeByteArray arr (off + 3) (unsafeWordToWord8 (byteFourFour codepoint))
      return (off + 4)
 where
  codepoint :: Word
  codepoint = fromIntegral (ord c)

  -- precondition: codepoint is less than 0x800
  byteTwoOne :: Word -> Word
  byteTwoOne w = unsafeShiftR w 6 .|. 0b11000000

  byteTwoTwo :: Word -> Word
  byteTwoTwo w = (w .&. 0b00111111) .|. 0b10000000

  -- precondition: codepoint is less than 0x1000
  byteThreeOne :: Word -> Word
  byteThreeOne w = unsafeShiftR w 12 .|. 0b11100000

  byteThreeTwo :: Word -> Word
  byteThreeTwo w = (0b00111111 .&. unsafeShiftR w 6) .|. 0b10000000

  byteThreeThree :: Word -> Word
  byteThreeThree w = (w .&. 0b00111111) .|. 0b10000000

  -- precondition: codepoint is less than 0x110000
  byteFourOne :: Word -> Word
  byteFourOne w = unsafeShiftR w 18 .|. 0b11110000

  byteFourTwo :: Word -> Word
  byteFourTwo w = (0b00111111 .&. unsafeShiftR w 12) .|. 0b10000000

  byteFourThree :: Word -> Word
  byteFourThree w = (0b00111111 .&. unsafeShiftR w 6) .|. 0b10000000

  byteFourFour :: Word -> Word
  byteFourFour w = (0b00111111 .&. w) .|. 0b10000000

int64BE :: Int64 -> Builder 8
int64BE (I64# i) = word64BE (W64# (wordToWord64# (int2Word# (int64ToInt# i))))

int32BE :: Int32 -> Builder 4
int32BE (I32# i) = word32BE (W32# (C.wordToWord32# (int2Word# (C.int32ToInt# i))))

int16BE :: Int16 -> Builder 2
int16BE (I16# i) = word16BE (W16# (C.wordToWord16# (int2Word# (C.int16ToInt# i))))

int64LE :: Int64 -> Builder 8
int64LE (I64# i) = word64LE (W64# (wordToWord64# (int2Word# (int64ToInt# i))))

int32LE :: Int32 -> Builder 4
int32LE (I32# i) = word32LE (W32# (C.wordToWord32# (int2Word# (C.int32ToInt# i))))

int16LE :: Int16 -> Builder 2
int16LE (I16# i) = word16LE (W16# (C.wordToWord16# (int2Word# (C.int16ToInt# i))))

word128LE :: Word128 -> Builder 16
word128LE (Word128 hi lo) = append (word64LE lo) (word64LE hi)

word128BE :: Word128 -> Builder 16
word128BE (Word128 hi lo) = append (word64BE hi) (word64BE lo)

word256LE :: Word256 -> Builder 32
word256LE (Word256 hi mhi mlo lo) = word64LE lo `append` word64LE mlo `append` word64LE mhi `append` word64LE hi

word256BE :: Word256 -> Builder 32
word256BE (Word256 hi mhi mlo lo) = word64BE hi `append` word64BE mhi `append` word64BE mlo `append` word64BE lo

{- | Requires exactly 8 bytes. Dump the octets of a 64-bit
word in a little-endian fashion.
-}
word64LE :: Word64 -> Builder 8
word64LE w = Unsafe.construct $ \arr off -> do
  writeByteArray arr (off + 7) (fromIntegral @Word64 @Word8 (unsafeShiftR w 56))
  writeByteArray arr (off + 6) (fromIntegral @Word64 @Word8 (unsafeShiftR w 48))
  writeByteArray arr (off + 5) (fromIntegral @Word64 @Word8 (unsafeShiftR w 40))
  writeByteArray arr (off + 4) (fromIntegral @Word64 @Word8 (unsafeShiftR w 32))
  writeByteArray arr (off + 3) (fromIntegral @Word64 @Word8 (unsafeShiftR w 24))
  writeByteArray arr (off + 2) (fromIntegral @Word64 @Word8 (unsafeShiftR w 16))
  writeByteArray arr (off + 1) (fromIntegral @Word64 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off) (fromIntegral @Word64 @Word8 w)
  pure (off + 8)

{- | Requires exactly 8 bytes. Dump the octets of a 64-bit
word in a big-endian fashion.
-}
word64BE :: Word64 -> Builder 8
word64BE w = Unsafe.construct $ \arr off -> do
  writeByteArray arr (off) (fromIntegral @Word64 @Word8 (unsafeShiftR w 56))
  writeByteArray arr (off + 1) (fromIntegral @Word64 @Word8 (unsafeShiftR w 48))
  writeByteArray arr (off + 2) (fromIntegral @Word64 @Word8 (unsafeShiftR w 40))
  writeByteArray arr (off + 3) (fromIntegral @Word64 @Word8 (unsafeShiftR w 32))
  writeByteArray arr (off + 4) (fromIntegral @Word64 @Word8 (unsafeShiftR w 24))
  writeByteArray arr (off + 5) (fromIntegral @Word64 @Word8 (unsafeShiftR w 16))
  writeByteArray arr (off + 6) (fromIntegral @Word64 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off + 7) (fromIntegral @Word64 @Word8 w)
  pure (off + 8)

{- | Requires exactly 4 bytes. Dump the octets of a 32-bit
word in a little-endian fashion.
-}
word32LE :: Word32 -> Builder 4
word32LE w = Unsafe.construct $ \arr off -> do
  writeByteArray arr (off + 3) (fromIntegral @Word32 @Word8 (unsafeShiftR w 24))
  writeByteArray arr (off + 2) (fromIntegral @Word32 @Word8 (unsafeShiftR w 16))
  writeByteArray arr (off + 1) (fromIntegral @Word32 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off) (fromIntegral @Word32 @Word8 w)
  pure (off + 4)

{- | Requires exactly 4 bytes. Dump the octets of a 32-bit
word in a big-endian fashion.
-}
word32BE :: Word32 -> Builder 4
word32BE w = Unsafe.construct $ \arr off -> do
  writeByteArray arr (off) (fromIntegral @Word32 @Word8 (unsafeShiftR w 24))
  writeByteArray arr (off + 1) (fromIntegral @Word32 @Word8 (unsafeShiftR w 16))
  writeByteArray arr (off + 2) (fromIntegral @Word32 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off + 3) (fromIntegral @Word32 @Word8 w)
  pure (off + 4)

{- | Requires exactly 2 bytes. Dump the octets of a 16-bit
word in a little-endian fashion.
-}
word16LE :: Word16 -> Builder 2
word16LE w = Unsafe.construct $ \arr off -> do
  writeByteArray arr (off + 1) (fromIntegral @Word16 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off) (fromIntegral @Word16 @Word8 w)
  pure (off + 2)

{- | Requires exactly 2 bytes. Dump the octets of a 16-bit
word in a big-endian fashion.
-}
word16BE :: Word16 -> Builder 2
word16BE w = Unsafe.construct $ \arr off -> do
  writeByteArray arr (off) (fromIntegral @Word16 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off + 1) (fromIntegral @Word16 @Word8 w)
  pure (off + 2)

word8 :: Word8 -> Builder 1
word8 w = Unsafe.construct $ \arr off -> do
  writeByteArray arr off w
  pure (off + 1)

unsafeWord8 :: Word8 -> Builder n
unsafeWord8 w = Unsafe.construct $ \arr off -> do
  writeByteArray arr off w
  pure (off + 1)

-- Reverse the bytes in the designated slice. This takes
-- an inclusive start offset and an inclusive end offset.
reverseBytes :: MutableByteArray s -> Int -> Int -> ST s ()
{-# INLINE reverseBytes #-}
reverseBytes arr begin end = go begin end
 where
  go ixA ixB =
    if ixA < ixB
      then do
        a :: Word8 <- readByteArray arr ixA
        b :: Word8 <- readByteArray arr ixB
        writeByteArray arr ixA b
        writeByteArray arr ixB a
        go (ixA + 1) (ixB - 1)
      else pure ()

c2w :: Char -> Word8
c2w = fromIntegral . ord

shrinkMutableByteArray :: MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  primitive_ (shrinkMutableByteArray# arr sz)

-- This is adapted from androider's code in https://stackoverflow.com/a/7097567
-- The checks for infinity and NaN have been removed. Note that this is a little
-- inaccurate. This is very visible when encoding a number like 2.25, which
-- is perfectly represented as an IEEE 754 floating point number but is goofed
-- up by this function.
doubleDec# ::
  forall s.
  Double# ->
  MutableByteArray# s ->
  Int# ->
  State# s ->
  (# State# s, Int# #)
doubleDec# d# marr# off# s0 =
  case unsafeIOToST (c_paste_double marr# off# d#) of
    ST f -> case f s0 of
      (# s1, I# r #) -> (# s1, r #)

-- Based on C code from https://stackoverflow.com/a/5558614
-- For numbers less than 1073741829, this gives a correct answer.
approxDiv10 :: Word -> Word
approxDiv10 !n = unsafeShiftR (0x1999999A * n) 32

unsafeWordToWord8 :: Word -> Word8
unsafeWordToWord8 (W# w) = W8# (C.wordToWord8# w)

foreign import ccall unsafe "bytebuild_paste_double"
  c_paste_double ::
    MutableByteArray# s -> Int# -> Double# -> IO Int
