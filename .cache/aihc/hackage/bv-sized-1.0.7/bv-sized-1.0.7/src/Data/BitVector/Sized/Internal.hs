{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Data.BitVector.Sized.Internal
Copyright   : (c) Galois Inc. 2018
License     : BSD-3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Internal hidden module containing all definitions for the 'BV' type.
-}

module Data.BitVector.Sized.Internal where

import Data.BitVector.Sized.Panic (panic)

-- Qualified imports
import qualified Data.Bits                  as B
import qualified Data.Bits.Bitwise          as B
import qualified Data.ByteString            as BS
import qualified Numeric                    as N
import qualified Data.Parameterized.NatRepr as P
import qualified Prelude

-- Unqualified imports
import Control.DeepSeq (NFData)
import Data.Char (intToDigit)
import Data.List (genericLength)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Parameterized ( NatRepr
                          , mkNatRepr
                          , natValue
                          , intValue
                          , addNat
                          , ShowF
                          , EqF(..)
                          , Hashable(..)
                          , Some(..)
                          , Pair(..)
                          )
import GHC.Generics (Generic)
import GHC.TypeLits (Nat, type(+), type(<=))
import Language.Haskell.TH.Lift (Lift)
import Numeric.Natural (Natural)
import Prelude hiding (abs, or, and, negate, concat, signum)
import System.Random.Stateful

----------------------------------------
-- Utility functions

-- | Check that a 'NatRepr' is representable as an 'Int'.
checkNatRepr :: NatRepr w -> a -> a
checkNatRepr = checkNatural . natValue

-- | Check that a 'Natural' is representable as an 'Int'.
checkNatural :: Natural -> a -> a
checkNatural n a = if n > (fromIntegral (maxBound :: Int) :: Natural)
  then panic "Data.BitVector.Sized.Internal.checkNatural"
       [show n ++ " not representable as Int"]
  else a


-- | Unsafe coercion from @Natural@ to @Int@.  We mostly use this to
--   interact with operations from "Data.Bits".  This should only be
--   called when we already know the input @Natural@ is small enough,
--   e.g., because we previously called @checkNatural@.
fromNatural :: Natural -> Int
fromNatural = fromIntegral

----------------------------------------
-- BitVector data type definitions

-- | Bitvector datatype, parameterized by width.
newtype BV (w :: Nat) :: Type where
  -- | We store the value as an 'Integer' rather than a 'Natural',
  -- since many of the operations on bitvectors rely on a two's
  -- complement representation. However, an invariant on the value is
  -- that it must always be positive.
  --
  -- Secondly, we maintain the invariant that any constructed BV value
  -- has a width whose value is representable in a Haskell @Int@.
  BV :: Integer -> BV w

  deriving ( Generic
           , NFData
           , Show
           , Read
           , Eq
           , Ord -- ^ Uses an unsigned ordering, but 'ule' and 'ult' are
                 -- preferred. We provide this instance to allow the use of 'BV'
                 -- in situations where an arbitrary ordering is required (e.g.,
                 -- as the keys in a map)
           , Lift)

instance ShowF BV

instance EqF BV where
  BV bv `eqF` BV bv' = bv == bv'

instance Hashable (BV w) where
  hashWithSalt salt (BV i) = hashWithSalt salt i

----------------------------------------
-- BV construction
-- | Internal function for masking the input integer *without*
-- checking that the width is representable as an 'Int'. We use this
-- instead of 'mkBV' whenever we already have some guarantee that the
-- width is legal.
mkBV' :: NatRepr w -> Integer -> BV w
mkBV' w x = BV (P.toUnsigned w x)

-- | Construct a bitvector with a particular width, where the width is
-- provided as an explicit `NatRepr` argument. The input 'Integer',
-- whether positive or negative, is silently truncated to fit into the
-- number of bits demanded by the return type. The width cannot be
-- arbitrarily large; it must be representable as an 'Int'.
--
-- >>> mkBV (knownNat @4) 10
-- BV 10
-- >>> mkBV (knownNat @2) 10
-- BV 2
-- >>> mkBV (knownNat @4) (-2)
-- BV 14
mkBV :: NatRepr w
     -- ^ Desired bitvector width
     -> Integer
     -- ^ Integer value to truncate to bitvector width
     -> BV w
mkBV w x = checkNatRepr w $ mkBV' w x

-- | Return 'Nothing' if the unsigned 'Integer' does not fit in the
-- required number of bits, otherwise return the input.
checkUnsigned :: NatRepr w
              -> Integer
              -> Maybe Integer
checkUnsigned w i = if i < 0 || i > P.maxUnsigned w
  then Nothing
  else Just i

-- | Like 'mkBV', but returns 'Nothing' if unsigned input integer cannot be
-- represented in @w@ bits.
mkBVUnsigned :: NatRepr w
             -- ^ Desired bitvector width
             -> Integer
             -- ^ Integer value
             -> Maybe (BV w)
mkBVUnsigned w x = checkNatRepr w $ BV <$> checkUnsigned w x

-- | Return 'Nothing if the signed 'Integer' does not fit in the
-- required number of bits, otherwise return an unsigned positive
-- integer that fits in @w@ bits.
signedToUnsigned :: 1 <= w => NatRepr w
                 -- ^ Width of output
                 -> Integer
                 -> Maybe Integer
signedToUnsigned w i = if i < P.minSigned w || i > P.maxSigned w
  then Nothing
  else Just $ if i < 0 then i + P.maxUnsigned w + 1 else i

-- | Like 'mkBV', but returns 'Nothing' if signed input integer cannot
-- be represented in @w@ bits.
mkBVSigned :: 1 <= w => NatRepr w
              -- ^ Desired bitvector width
           -> Integer
           -- ^ Integer value
           -> Maybe (BV w)
mkBVSigned w x = checkNatRepr w $ BV <$> signedToUnsigned w x

-- | The zero bitvector of any width.
zero :: NatRepr w -> BV w
zero w = checkNatRepr w $ BV 0

-- | The bitvector with value 1, of any positive width.
one :: 1 <= w => NatRepr w -> BV w
one w = checkNatRepr w $ BV 1

-- | The bitvector whose value is its own width, of any width.
width :: NatRepr w -> BV w
width w = checkNatRepr w $ BV (intValue w)

-- | The minimum unsigned value for bitvector with given width (always 0).
minUnsigned :: NatRepr w -> BV w
minUnsigned w = checkNatRepr w $ BV 0

-- | The maximum unsigned value for bitvector with given width.
maxUnsigned :: NatRepr w -> BV w
maxUnsigned w = checkNatRepr w $ BV (P.maxUnsigned w)

-- | The minimum value for bitvector in two's complement with given width.
minSigned :: 1 <= w => NatRepr w -> BV w
minSigned w = mkBV w (P.minSigned w)

-- | The maximum value for bitvector in two's complement with given width.
maxSigned :: 1 <= w => NatRepr w -> BV w
maxSigned w = checkNatRepr w $ BV (P.maxSigned w)

-- | @unsignedClamp w i@ rounds @i@ to the nearest value between @0@
-- and @2^w - 1@ (inclusive).
unsignedClamp :: NatRepr w -> Integer -> BV w
unsignedClamp w x = checkNatRepr w $
  if | x < P.minUnsigned w -> BV (P.minUnsigned w)
     | x > P.maxUnsigned w -> BV (P.maxUnsigned w)
     | otherwise -> BV x

-- | @signedClamp w i@ rounds @i@ to the nearest value between
-- @-2^(w-1)@ and @2^(w-1) - 1@ (inclusive).
signedClamp :: 1 <= w => NatRepr w -> Integer -> BV w
signedClamp w x = checkNatRepr w $
  if | x < P.minSigned w -> mkBV' w (P.minSigned w)
     | x > P.maxSigned w -> BV (P.maxSigned w)
     | otherwise -> mkBV' w x

----------------------------------------
-- Construction from fixed-width data types

-- | Construct a 'BV' from a 'Bool'.
bool :: Bool -> BV 1
bool True = BV 1
bool False = BV 0

-- | Construct a 'BV' from a 'Word8'.
word8 :: Word8 -> BV 8
word8 = BV . toInteger

-- | Construct a 'BV' from a 'Word16'.
word16 :: Word16 -> BV 16
word16 = BV . toInteger

-- | Construct a 'BV' from a 'Word32'.
word32 :: Word32 -> BV 32
word32 = BV . toInteger

-- | Construct a 'BV' from a 'Word64'.
word64 :: Word64 -> BV 64
word64 = BV . toInteger

-- | Construct a 'BV' from an 'Int8'.
int8 :: Int8 -> BV 8
int8 = word8 . (fromIntegral :: Int8 -> Word8)

-- | Construct a 'BV' from an 'Int16'.
int16 :: Int16 -> BV 16
int16 = word16 . (fromIntegral :: Int16 -> Word16)

-- | Construct a 'BV' from an 'Int32'.
int32 :: Int32 -> BV 32
int32 = word32 . (fromIntegral :: Int32 -> Word32)

-- | Construct a 'BV' from an 'Int64'.
int64 :: Int64 -> BV 64
int64 = word64 . (fromIntegral :: Int64 -> Word64)

-- | Construct a 'BV' from a list of bits, in big endian order (bits
-- with lower value index in the list are mapped to higher order bits
-- in the output bitvector). Return the resulting 'BV' along with its
-- width.
--
-- >>> case bitsBE [True, False] of p -> (fstPair p, sndPair p)
-- (2,BV 2)
bitsBE :: [Bool] -> Pair NatRepr BV
bitsBE bs = case mkNatRepr (fromInteger (genericLength bs)) of
  Some w -> checkNatRepr w $ Pair w (BV (B.fromListBE bs))

-- | Construct a 'BV' from a list of bits, in little endian order
-- (bits with lower value index in the list are mapped to lower order
-- bits in the output bitvector). Return the resulting 'BV' along
-- with its width.
--
-- >>> case bitsLE [True, False] of p -> (fstPair p, sndPair p)
-- (2,BV 1)
bitsLE :: [Bool] -> Pair NatRepr BV
bitsLE bs = case mkNatRepr (fromInteger (genericLength bs)) of
  Some w -> checkNatRepr w $ Pair w (BV (B.fromListLE bs))

-- | Convert a 'ByteString' (big-endian) of length @n@ to an 'Integer'
-- with @8*n@ bits. This function uses a balanced binary fold to
-- achieve /O(n log n)/ total memory allocation and run-time, in
-- contrast to the /O(n^2)/ that would be required by a naive
-- left-fold.
bytestringToIntegerBE :: BS.ByteString -> Integer
bytestringToIntegerBE bs
  | l == 0 = 0
  | l == 1 = toInteger (BS.head bs)
  | otherwise = x1 `B.shiftL` (l2 * 8) B..|. x2
  where
    l = BS.length bs
    l1 = l `div` 2
    l2 = l - l1
    (bs1, bs2) = BS.splitAt l1 bs
    x1 = bytestringToIntegerBE bs1
    x2 = bytestringToIntegerBE bs2

bytestringToIntegerLE :: BS.ByteString -> Integer
bytestringToIntegerLE bs
  | l == 0 = 0
  | l == 1 = toInteger (BS.head bs)
  | otherwise = x2 `B.shiftL` (l1 * 8) B..|. x1
  where
    l = BS.length bs
    l1 = l `div` 2
    (bs1, bs2) = BS.splitAt l1 bs
    x1 = bytestringToIntegerLE bs1
    x2 = bytestringToIntegerLE bs2

-- | Construct a 'BV' from a big-endian bytestring.
--
-- >>> case bytestringBE (BS.pack [0, 1, 1]) of p -> (fstPair p, sndPair p)
-- (24,BV 257)
bytestringBE :: BS.ByteString -> Pair NatRepr BV
bytestringBE bs = case mkNatRepr (8*fromIntegral (BS.length bs)) of
  Some w -> checkNatRepr w $ Pair w (BV (bytestringToIntegerBE bs))

-- | Construct a 'BV' from a little-endian bytestring.
--
-- >>> case bytestringLE (BS.pack [0, 1, 1]) of p -> (fstPair p, sndPair p)
-- (24,BV 65792)
bytestringLE :: BS.ByteString -> Pair NatRepr BV
bytestringLE bs = case mkNatRepr (8*fromIntegral (BS.length bs)) of
  Some w -> checkNatRepr w $ Pair w (BV (bytestringToIntegerLE bs))

-- | Construct a 'BV' from a list of bytes, in big endian order (bytes
-- with lower value index in the list are mapped to higher order bytes
-- in the output bitvector).
--
-- >>> case bytesBE [0, 1, 1] of p -> (fstPair p, sndPair p)
-- (24,BV 257)
bytesBE :: [Word8] -> Pair NatRepr BV
bytesBE = bytestringBE . BS.pack

-- | Construct a 'BV' from a list of bytes, in little endian order
-- (bytes with lower value index in the list are mapped to lower
-- order bytes in the output bitvector).
--
-- >>> case bytesLE [0, 1, 1] of p -> (fstPair p, sndPair p)
-- (24,BV 65792)
bytesLE :: [Word8] -> Pair NatRepr BV
bytesLE = bytestringLE . BS.pack

----------------------------------------
-- BitVector -> Integer functions

-- | Unsigned interpretation of a bitvector as a positive Integer.
asUnsigned :: BV w -> Integer
asUnsigned (BV x) = x

-- | Signed interpretation of a bitvector as an Integer.
asSigned :: (1 <= w) => NatRepr w -> BV w -> Integer
asSigned w (BV x) =
  -- NB, fromNatural is OK here because we maintain the invariant that
  --  any existing BV value has a representable width
  let wInt = fromNatural (natValue w) in
  if B.testBit x (wInt - 1)
  then x - B.bit wInt
  else x

-- | Unsigned interpretation of a bitvector as a Natural.
asNatural :: BV w -> Natural
asNatural = (fromInteger :: Integer -> Natural) . asUnsigned

-- | Convert a bitvector to a list of bits, in big endian order
-- (higher order bits in the bitvector are mapped to lower indices in
-- the output list).
--
-- >>> asBitsBE (knownNat @5) (mkBV knownNat 0b1101)
-- [False,True,True,False,True]
asBitsBE :: NatRepr w -> BV w -> [Bool]
asBitsBE w bv = [ testBit' i bv | i <- fromInteger <$> [wi - 1, wi - 2 .. 0] ]
  where wi = intValue w

-- | Convert a bitvector to a list of bits, in little endian order
-- (lower order bits in the bitvector are mapped to lower indices in
-- the output list).
--
-- >>> asBitsLE (knownNat @5) (mkBV knownNat 0b1101)
-- [True,False,True,True,False]
asBitsLE :: NatRepr w -> BV w -> [Bool]
asBitsLE w bv = [ testBit' i bv | i <- fromInteger <$> [0 .. wi - 1] ]
  where wi = intValue w

integerToBytesBE :: Natural
                 -- ^ number of bytes
                 -> Integer
                 -> [Word8]
integerToBytesBE n x =
  [ fromIntegral (x `B.shiftR` (8*ix)) | ix <- [ni-1, ni-2 .. 0] ]
  where ni = fromIntegral n

integerToBytesLE :: Natural
                 -- ^ number of bytes
                 -> Integer
                 -> [Word8]
integerToBytesLE n x =
  [ fromIntegral (x `B.shiftR` (8*ix)) | ix <- [0 .. ni-1] ]
  where ni = fromIntegral n

-- | Convert a bitvector to a list of bytes, in big endian order
-- (higher order bytes in the bitvector are mapped to lower indices in
-- the output list). Return 'Nothing' if the width is not a multiple
-- of 8.
--
-- >>> asBytesBE (knownNat @32) (mkBV knownNat 0xaabbccdd)
-- Just [170,187,204,221]
asBytesBE :: NatRepr w -> BV w -> Maybe [Word8]
asBytesBE w (BV x)
  | natValue w `mod` 8 == 0 = Just $ integerToBytesBE (natValue w `div` 8) x
  | otherwise = Nothing

-- | Convert a bitvector to a list of bytes, in little endian order
-- (lower order bytes in the bitvector are mapped to lower indices in
-- the output list). Return 'Nothing' if the width is not a multiple
-- of 8.
--
-- >>> asBytesLE (knownNat @32) (mkBV knownNat 0xaabbccdd)
-- Just [221,204,187,170]
asBytesLE :: NatRepr w -> BV w -> Maybe [Word8]
asBytesLE w (BV x)
  | natValue w `mod` 8 == 0 = Just $ integerToBytesLE (natValue w `div` 8) x
  | otherwise = Nothing

-- | 'asBytesBE', but for bytestrings.
asBytestringBE :: NatRepr w -> BV w -> Maybe BS.ByteString
asBytestringBE w bv = BS.pack <$> asBytesBE w bv

-- | 'asBytesLE', but for bytestrings.
asBytestringLE :: NatRepr w -> BV w -> Maybe BS.ByteString
asBytestringLE w bv = BS.pack <$> asBytesLE w bv

----------------------------------------
-- BV w operations (fixed width)

-- | Bitwise and.
and :: BV w -> BV w -> BV w
and (BV x) (BV y) = BV (x B..&. y)

-- | Bitwise or.
or :: BV w -> BV w -> BV w
or (BV x) (BV y) = BV (x B..|. y)

-- | Bitwise xor.
xor :: BV w -> BV w -> BV w
xor (BV x) (BV y) = BV (x `B.xor` y)

-- | Bitwise complement (flip every bit).
complement :: NatRepr w -> BV w -> BV w
complement w (BV x) = mkBV' w (B.complement x)


-- | Clamp shift amounts to the word width and
--   coerce to an @Int@
shiftAmount :: NatRepr w -> Natural -> Int
shiftAmount w shf = fromNatural (min (natValue w) shf)

-- | Left shift by positive 'Natural'.
shl :: NatRepr w -> BV w -> Natural -> BV w
shl w (BV x) shf = mkBV' w (x `B.shiftL` shiftAmount w shf)

-- | Right arithmetic shift by positive 'Natural'.
ashr :: (1 <= w) => NatRepr w -> BV w -> Natural -> BV w
ashr w bv shf = mkBV' w (asSigned w bv `B.shiftR` shiftAmount w shf)

-- | Right logical shift by positive 'Natural'.
lshr :: NatRepr w -> BV w -> Natural -> BV w
lshr w (BV x) shf =
  -- Shift right (logical by default since the value is positive). No
  -- need to truncate bits, since the result is guaranteed to occupy
  -- no more bits than the input.
  BV (x `B.shiftR` shiftAmount w shf)

-- | Bitwise rotate left.
rotateL :: NatRepr w -> BV w -> Natural -> BV w
rotateL w bv rot' = leftChunk `or` rightChunk
  where rot = if wNatural == 0 then 0 else rot' `mod` wNatural
        leftChunk = shl w bv rot
        rightChunk = lshr w bv (wNatural - rot)
        wNatural = natValue w

-- | Bitwise rotate right.
rotateR :: NatRepr w -> BV w -> Natural -> BV w
rotateR w bv rot' = leftChunk `or` rightChunk
  where rot = if wNatural == 0 then 0 else rot' `mod` wNatural
        rightChunk = lshr w bv rot
        leftChunk = shl w bv (wNatural - rot)
        wNatural = natValue w

-- | The 'BV' that has a particular bit set, and is 0 everywhere
-- else.
bit :: ix+1 <= w
    => NatRepr w
    -- ^ Desired output width
    -> NatRepr ix
    -- ^ Index of bit to set
    -> BV w
bit w ix =
  checkNatRepr w $
    -- NB fromNatural is OK here because of the (ix+1<w) constraint
    BV (B.bit (fromNatural (natValue ix)))

-- | Like 'bit', but without the requirement that the index bit refers
-- to an actual bit in the output 'BV'. If it is out of range, just
-- silently return the zero bitvector.
bit' :: NatRepr w
     -- ^ Desired output width
     -> Natural
     -- ^ Index of bit to set
     -> BV w
bit' w ix
  | ix < natValue w = checkNatRepr w $ mkBV' w (B.bit (fromNatural ix))
  | otherwise = zero w

-- | @setBit bv ix@ is the same as @or bv (bit knownNat ix)@.
setBit :: ix+1 <= w
       => NatRepr ix
       -- ^ Index of bit to set
       -> BV w
       -- ^ Original bitvector
       -> BV w
setBit ix bv =
  -- NB, fromNatural is OK because of the (ix+1 <= w) constraint
  or bv (BV (B.bit (fromNatural (natValue ix))))

-- | Like 'setBit', but without the requirement that the index bit
-- refers to an actual bit in the input 'BV'. If it is out of range,
-- just silently return the original input.
setBit' :: NatRepr w
        -- ^ Desired output width
        -> Natural
        -- ^ Index of bit to set
        -> BV w
        -- ^ Original bitvector
        -> BV w
setBit' w ix bv
  | ix < natValue w = or bv (BV (B.bit (fromNatural ix)))
  | otherwise = bv

-- | @clearBit w bv ix@ is the same as @and bv (complement (bit w ix))@.
clearBit :: ix+1 <= w
         => NatRepr w
         -- ^ Desired output width
         -> NatRepr ix
         -- ^ Index of bit to clear
         -> BV w
         -- ^ Original bitvector
         -> BV w
clearBit w ix bv =
  -- NB, fromNatural is OK because of the (ix+1<=w) constraint
  and bv (complement w (BV (B.bit (fromNatural (natValue ix)))))


-- | Like 'clearBit', but without the requirement that the index bit
-- refers to an actual bit in the input 'BV'. If it is out of range,
-- just silently return the original input.
clearBit' :: NatRepr w
          -- ^ Desired output width
          -> Natural
          -- ^ Index of bit to clear
          -> BV w
          -- ^ Original bitvector
          -> BV w
clearBit' w ix bv
  | ix < natValue w = and bv (complement w (BV (B.bit (fromNatural ix))))
  | otherwise = bv

-- | @complementBit bv ix@ is the same as @xor bv (bit knownNat ix)@.
complementBit :: ix+1 <= w
              => NatRepr ix
              -- ^ Index of bit to flip
              -> BV w
              -- ^ Original bitvector
              -> BV w
complementBit ix bv =
  -- NB, fromNatural is OK because of (ix+1 <= w) constraint
  xor bv (BV (B.bit (fromNatural (natValue ix))))

-- | Like 'complementBit', but without the requirement that the index
-- bit refers to an actual bit in the input 'BV'. If it is out of
-- range, just silently return the original input.
complementBit' :: NatRepr w
               -- ^ Desired output width
               -> Natural
               -- ^ Index of bit to flip
               -> BV w
               -- ^ Original bitvector
               -> BV w
complementBit' w ix bv
  | ix < natValue w = xor bv (BV (B.bit (fromNatural ix)))
  | otherwise = bv

-- | Test if a particular bit is set.
testBit :: ix+1 <= w => NatRepr ix -> BV w -> Bool
testBit ix (BV x) = B.testBit x (fromNatural (natValue ix))

-- | Like 'testBit', but takes a 'Natural' for the bit index. If the
-- index is out of bounds, return 'False'.
testBit' :: Natural -> BV w -> Bool
testBit' ix (BV x)
  -- NB, If the index is larger than the maximum representable 'Int',
  -- this function should be 'False' by construction of 'BV'.
  | ix > fromIntegral (maxBound :: Int) = False
  | otherwise = B.testBit x (fromNatural ix)

-- | Get the number of 1 bits in a 'BV'.
popCount :: BV w -> BV w
popCount (BV x) = BV (toInteger (B.popCount x))

-- | Count trailing zeros in a 'BV'.
ctz :: NatRepr w -> BV w -> BV w
ctz w (BV x) = BV (go 0)
  where go !i | i < intValue w &&
                not (B.testBit x (fromInteger i)) = go (i+1)
              | otherwise = i

-- | Count leading zeros in a 'BV'.
clz :: NatRepr w -> BV w -> BV w
clz w (BV x) = BV (go 0)
 where go !i | i < intValue w &&
               not (B.testBit x (fromInteger (intValue w - i - 1))) =
                 go (i+1)
             | otherwise = i

-- | Truncate a bitvector to a particular width given at runtime,
-- while keeping the type-level width constant.
truncBits :: Natural -> BV w -> BV w
truncBits b (BV x) = checkNatural b $ BV (x B..&. (B.bit (fromNatural b) - 1))

----------------------------------------
-- BV w arithmetic operations (fixed width)

-- | Bitvector add.
add :: NatRepr w -> BV w -> BV w -> BV w
add w (BV x) (BV y) = mkBV' w (x+y)

-- | Bitvector subtract.
sub :: NatRepr w -> BV w -> BV w -> BV w
sub w (BV x) (BV y) = mkBV' w (x-y)

-- | Bitvector multiply.
mul :: NatRepr w -> BV w -> BV w -> BV w
mul w (BV x) (BV y) = mkBV' w (x*y)

-- | Bitvector division (unsigned). Rounds to zero. Division by zero
-- yields a runtime error.
uquot :: BV w -> BV w -> BV w
uquot (BV x) (BV y) = BV (x `quot` y)

-- | Bitvector remainder after division (unsigned), when rounded to
-- zero. Division by zero yields a runtime error.
urem :: BV w -> BV w -> BV w
urem (BV x) (BV y) = BV (x `rem` y)

-- | 'uquot' and 'urem' returned as a pair.
uquotRem :: BV w -> BV w -> (BV w, BV w)
uquotRem bv1 bv2 = (uquot bv1 bv2, urem bv1 bv2)

-- | Bitvector division (signed). Rounds to zero. Division by zero
-- yields a runtime error.
squot :: (1 <= w) => NatRepr w -> BV w -> BV w -> BV w
squot w bv1 bv2 = mkBV' w (x `quot` y)
  where x = asSigned w bv1
        y = asSigned w bv2

-- | Bitvector remainder after division (signed), when rounded to
-- zero. Division by zero yields a runtime error.
srem :: (1 <= w) => NatRepr w -> BV w -> BV w -> BV w
srem w bv1 bv2 = mkBV' w (x `rem` y)
  where x = asSigned w bv1
        y = asSigned w bv2

-- | 'squot' and 'srem' returned as a pair.
squotRem :: (1 <= w) => NatRepr w -> BV w -> BV w -> (BV w, BV w)
squotRem w bv1 bv2 = (squot w bv1 bv2, srem w bv1 bv2)

-- | Bitvector division (signed). Rounds to negative infinity. Division
-- by zero yields a runtime error.
sdiv :: (1 <= w) => NatRepr w -> BV w -> BV w -> BV w
sdiv w bv1 bv2 = mkBV' w (x `div` y)
  where x = asSigned w bv1
        y = asSigned w bv2

-- | Bitvector remainder after division (signed), when rounded to
-- negative infinity. Division by zero yields a runtime error.
smod :: (1 <= w) => NatRepr w -> BV w -> BV w -> BV w
smod w bv1 bv2 = mkBV' w (x `mod` y)
  where x = asSigned w bv1
        y = asSigned w bv2

-- | 'sdiv' and 'smod' returned as a pair.
sdivMod :: (1 <= w) => NatRepr w -> BV w -> BV w -> (BV w, BV w)
sdivMod w bv1 bv2 = (sdiv w bv1 bv2, smod w bv1 bv2)

-- | Bitvector absolute value.  Returns the 2's complement
--   magnitude of the bitvector.
abs :: (1 <= w) => NatRepr w -> BV w -> BV w
abs w bv = mkBV' w (Prelude.abs (asSigned w bv))

-- | 2's complement bitvector negation.
negate :: NatRepr w -> BV w -> BV w
negate w (BV x) = mkBV' w (-x)

-- | Get the sign bit as a 'BV'.
signBit :: 1 <= w => NatRepr w -> BV w -> BV w
signBit w bv@(BV _) = lshr w bv (natValue w - 1) `and` BV 1

-- | Return 1 if positive, -1 if negative, and 0 if 0.
signum :: 1 <= w => NatRepr w -> BV w -> BV w
signum w bv = mkBV' w (Prelude.signum (asSigned w bv))

-- | Signed less than.
slt :: (1 <= w) => NatRepr w -> BV w -> BV w -> Bool
slt w bv1 bv2 = asSigned w bv1 < asSigned w bv2

-- | Signed less than or equal.
sle :: (1 <= w) => NatRepr w -> BV w -> BV w -> Bool
sle w bv1 bv2 = asSigned w bv1 <= asSigned w bv2

-- | Unsigned less than.
ult :: BV w -> BV w -> Bool
ult bv1 bv2 = asUnsigned bv1 < asUnsigned bv2

-- | Unsigned less than or equal.
ule :: BV w -> BV w -> Bool
ule bv1 bv2 = asUnsigned bv1 <= asUnsigned bv2

-- | Unsigned minimum of two bitvectors.
umin :: BV w -> BV w -> BV w
umin (BV x) (BV y) = if x < y then BV x else BV y

-- | Unsigned maximum of two bitvectors.
umax :: BV w -> BV w -> BV w
umax (BV x) (BV y) = if x < y then BV y else BV x

-- | Signed minimum of two bitvectors.
smin :: (1 <= w) => NatRepr w -> BV w -> BV w -> BV w
smin w bv1 bv2 = if asSigned w bv1 < asSigned w bv2 then bv1 else bv2

-- | Signed maximum of two bitvectors.
smax :: (1 <= w) => NatRepr w -> BV w -> BV w -> BV w
smax w bv1 bv2 = if asSigned w bv1 < asSigned w bv2 then bv2 else bv1

----------------------------------------
-- Width-changing operations

-- | Concatenate two bitvectors. The first argument gets placed in the
-- higher order bits.
--
-- >>> concat knownNat (mkBV (knownNat @3) 0b001) (mkBV (knownNat @2) 0b10)
-- BV 6
-- >>> :type it
-- it :: BV 5
concat :: NatRepr w
       -- ^ Width of higher-order bits
       -> NatRepr w'
       -- ^ Width of lower-order bits
       -> BV w
       -- ^ Higher-order bits
       -> BV w'
       -- ^ Lower-order bits
       -> BV (w+w')
concat w w' (BV hi) (BV lo) = checkNatRepr (w `addNat` w') $
  BV ((hi `B.shiftL` fromNatural (natValue w')) B..|. lo)

-- | Slice out a smaller bitvector from a larger one.
--
-- >>> select (knownNat @4) (knownNat @1) (mkBV (knownNat @12) 0b110010100110)
-- BV 3
-- >>> :type it
-- it :: BV 4
select :: ix + w' <= w
       => NatRepr ix
       -- ^ Index to start selecting from
       -> NatRepr w'
       -- ^ Desired output width
       -> BV w
       -- ^ Bitvector to select from
       -> BV w'
select ix w' (BV x) = mkBV' w' xShf
  -- NB fromNatural is OK because of (ix + w' <= w) constraint
  where xShf = x `B.shiftR` fromNatural (natValue ix)

-- | Like 'select', but takes a 'Natural' as the index to start
-- selecting from. Neither the index nor the output width is checked
-- to ensure the resulting 'BV' lies entirely within the bounds of the
-- original bitvector. Any bits "selected" from beyond the bounds of
-- the input bitvector will be 0.
--
-- >>> select' (knownNat @4) 9 (mkBV (knownNat @12) 0b110010100110)
-- BV 6
-- >>> :type it
-- it :: BV 4
select' :: Natural
        -- ^ Index to start selecting from
        -> NatRepr w'
        -- ^ Desired output width
        -> BV w
        -- ^ Bitvector to select from
        -> BV w'
select' ix w' (BV x)
  | toInteger ix < toInteger (maxBound :: Int) = mkBV w' (x `B.shiftR` fromNatural ix)
  | otherwise = zero w'

-- | Zero-extend a bitvector to one of strictly greater width.
--
-- >>> zext (knownNat @8) (mkBV (knownNat @4) 0b1101)
-- BV 13
-- >>> :type it
-- it :: BV 8
zext :: w + 1 <= w'
     => NatRepr w'
     -- ^ Desired output width
     -> BV w
     -- ^ Bitvector to extend
     -> BV w'
zext w (BV x) = checkNatRepr w $ BV x

-- | Sign-extend a bitvector to one of strictly greater width.
sext :: (1 <= w, w + 1 <= w')
     => NatRepr w
     -- ^ Width of input bitvector
     -> NatRepr w'
     -- ^ Desired output width
     -> BV w
     -- ^ Bitvector to extend
     -> BV w'
sext w w' bv = mkBV w' (asSigned w bv)

-- | Truncate a bitvector to one of strictly smaller width.
trunc :: w' + 1 <= w
      => NatRepr w'
      -- ^ Desired output width
      -> BV w
      -- ^ Bitvector to truncate
      -> BV w'
trunc w' (BV x) = mkBV' w' x

-- | Like 'trunc', but allows the input width to be greater than or
-- equal to the output width, in which case it just performs a zero
-- extension.
trunc' :: NatRepr w'
       -- ^ Desired output width
       -> BV w
       -- ^ Bitvector to truncate
       -> BV w'
trunc' w' (BV x) = mkBV w' x
{-# DEPRECATED trunc' "Use zresize instead" #-}

-- | Resizes a bitvector. If @w' > w@, perform a zero extension.
zresize :: NatRepr w'
        -- ^ Desired output width
        -> BV w
        -- ^ Bitvector to resize
        -> BV w'
zresize w' (BV x) = mkBV w' x

-- | Resizes a bitvector. If @w' > w@, perform a signed extension.
sresize :: 1 <= w
        => NatRepr w
        -- ^ Width of input vector
        -> NatRepr w'
        -- ^ Desired output width
        -> BV w
        -- ^ Bitvector to resize
        -> BV w'
sresize w w' bv = mkBV w' (asSigned w bv)

-- | Wide multiply of two bitvectors.
mulWide :: NatRepr w -> NatRepr w' -> BV w -> BV w' -> BV (w+w')
mulWide w w' (BV x) (BV y) = checkNatRepr (w `addNat` w') $ BV (x*y)

----------------------------------------
-- Enum functions

-- | Unsigned successor. @succUnsigned w (maxUnsigned w)@ returns 'Nothing'.
succUnsigned :: NatRepr w -> BV w -> Maybe (BV w)
succUnsigned w (BV x) =
  if x == P.maxUnsigned w
  then Nothing
  else Just (BV (x+1))

-- | Signed successor. @succSigned w (maxSigned w)@ returns 'Nothing'.
succSigned :: 1 <= w => NatRepr w -> BV w -> Maybe (BV w)
succSigned w (BV x) =
  if x == P.maxSigned w
  then Nothing
  else Just (mkBV' w (x+1))

-- | Unsigned predecessor. @predUnsigned w zero@ returns 'Nothing'.
predUnsigned :: NatRepr w -> BV w -> Maybe (BV w)
predUnsigned w (BV x) =
  if x == P.minUnsigned w
  then Nothing
  else Just (BV (x-1))

-- | Signed predecessor. @predSigned w (minSigned w)@ returns 'Nothing'.
predSigned :: 1 <= w => NatRepr w -> BV w -> Maybe (BV w)
predSigned w bv@(BV x) =
  if bv == minSigned w
  then Nothing
  else Just (mkBV' w (x-1))

-- | List of all unsigned bitvectors from a lower to an upper bound,
-- inclusive.
enumFromToUnsigned :: BV w
                   -- ^ Lower bound
                   -> BV w
                   -- ^ Upper bound
                   -> [BV w]
enumFromToUnsigned bv1 bv2 = BV <$> [asUnsigned bv1 .. asUnsigned bv2]

-- | List of all signed bitvectors from a lower to an upper bound,
-- inclusive.
enumFromToSigned :: 1 <= w => NatRepr w
                 -> BV w
                 -- ^ Lower bound
                 -> BV w
                 -- ^ Upper bound
                 -> [BV w]
enumFromToSigned w bv1 bv2 =
  BV . fromJust . signedToUnsigned w <$> [asSigned w bv1 .. asSigned w bv2]

----------------------------------------
-- Generating random bitvectors

-- | Generates a bitvector uniformly distributed over all possible values for a
-- given width. (See 'System.Random.Stateful.uniformM').
uniformM :: StatefulGen g m => NatRepr w -> g -> m (BV w)
uniformM w g = BV <$> uniformRM (P.minUnsigned w, P.maxUnsigned w) g

-- | Generates a bitvector uniformly distributed over the provided range
-- (interpreted as a range of /unsigned/ bitvectors), which is interpreted as
-- inclusive in the lower and upper bound. (See
-- 'System.Random.Stateful.uniformRM').
uUniformRM :: StatefulGen g m => (BV w, BV w) -> g -> m (BV w)
uUniformRM (lo, hi) g =
  let loI = asUnsigned lo
      hiI = asUnsigned hi
  in BV <$> uniformRM (loI, hiI) g

-- | Generates a bitvector uniformly distributed over the provided range
-- (interpreted as a range of /signed/ bitvectors), which is interpreted as
-- inclusive in the lower and upper bound. (See
-- 'System.Random.Stateful.uniformRM').
sUniformRM :: (StatefulGen g m, 1 <= w) => NatRepr w -> (BV w, BV w) -> g -> m (BV w)
sUniformRM w (lo, hi) g =
  let loI = asSigned w lo
      hiI = asSigned w hi
  in mkBV w <$> uniformRM (loI, hiI) g

----------------------------------------
-- Pretty printing

-- | Pretty print in hex
ppHex :: NatRepr w -> BV w -> String
ppHex w (BV x) = "0x" ++ N.showHex x "" ++ ":" ++ ppWidth w

-- | Pretty print in binary
ppBin :: NatRepr w -> BV w -> String
ppBin w (BV x) = "0b" ++ N.showIntAtBase 2 intToDigit x "" ++ ":" ++ ppWidth w

-- | Pretty print in octal
ppOct :: NatRepr w -> BV w -> String
ppOct w (BV x) = "0o" ++ N.showOct x "" ++ ":" ++ ppWidth w

-- | Pretty print in decimal
ppDec :: NatRepr w -> BV w -> String
ppDec w (BV x) = show x ++ ":" ++ ppWidth w

ppWidth :: NatRepr w -> String
ppWidth w = "[" ++ show (natValue w) ++ "]"
