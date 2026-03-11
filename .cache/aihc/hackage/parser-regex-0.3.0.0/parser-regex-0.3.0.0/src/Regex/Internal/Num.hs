{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Regex.Internal.Num
  ( mkNaturalDec
  , mkWordDecN
  , mkWordRangeDec
  , mkNaturalHex
  , mkWordHexN
  , mkWordRangeHex
  , mkSignedInteger
  , mkSignedIntRange
  ) where

#include "MachDeps.h"

import Control.Applicative ((<|>), empty)
import qualified Control.Applicative as Ap
import Control.Monad (replicateM_, void)
import Data.Bits ((.&.), countLeadingZeros, unsafeShiftL, unsafeShiftR)
import Numeric.Natural (Natural)
#ifdef __GLASGOW_HASKELL__
import Data.Primitive.PrimArray
  (PrimArray(..), newPrimArray, runPrimArray, writePrimArray)
import qualified GHC.Num.Natural as Nat
#endif

import Regex.Internal.Regex (RE)
import qualified Regex.Internal.Regex as R

mkNaturalDec
  :: (Word -> Word -> RE c Word)  -- Decimal digit range
  -> RE c Natural
mkNaturalDec d =
      0 <$ d 0 0
  <|> Ap.liftA2 finishDec (d 1 9) (R.foldlMany' stepDec state0 (d 0 9))
  where
    state0 = NatParseState 0 1 WNil
    -- Start with len=1, it's reserved for the leading digit
{-# INLINE mkNaturalDec #-}

mkNaturalHex
  :: (Word -> Word -> RE c Word)  -- Hexadecimal digit range
  -> RE c Natural
mkNaturalHex d =
      0 <$ d 0 0
  <|> Ap.liftA2 finishHex (d 1 15) (R.foldlMany' stepHex state0 (d 0 15))
  where
    state0 = NatParseState 0 1 WNil
    -- Start with len=1, it's reserved for the leading digit
{-# INLINE mkNaturalHex #-}

mkSignedInteger :: RE c minus -> RE c plus -> RE c Natural -> RE c Integer
mkSignedInteger minus plus rnat = signed <*> rnat
  where
    signed = negate . fromIntegral <$ minus
         <|> fromIntegral <$ plus
         <|> pure fromIntegral

mkWordDecN
  :: (Word -> Word -> RE c Word)  -- Decimal digit range
  -> Int
  -> RE c Word
mkWordDecN d n0
  | n0 <= 0 = empty
  | maxBoundWordDecLen <= n0 =
      replicateM_ (n0 - maxBoundWordDecLen) d00 *>
      (    d00 *> go (maxBoundWordDecLen - 1)
       <|> mkWordRangeDec d (pow10 safeWordDecLen, maxBound) )
  | otherwise = go n0
  where
    go 1 = d09
    go n = R.liftA2' (\x y -> x * 10 + y) (go (n-1)) d09
    d00 = d 0 0
    d09 = d 0 9
{-# INLINE mkWordDecN #-}

mkWordHexN
  :: (Word -> Word -> RE c Word)  -- Hexadecimal digit range
  -> Int
  -> RE c Word
mkWordHexN d n0
  | n0 <= 0 = empty
  | maxBoundWordHexLen < n0 =
      replicateM_ (n0 - maxBoundWordHexLen) d00 *> go maxBoundWordHexLen
  | otherwise = go n0
  where
    go 1 = d0f
    go n = R.liftA2' (\x y -> x * 16 + y) (go (n-1)) d0f
    d00 = d 0 0
    d0f = d 0 15
{-# INLINE mkWordHexN #-}

mkWordRangeDec
  :: (Word -> Word -> RE c Word)  -- Decimal digit range
  -> (Word, Word)  -- Low high
  -> RE c Word
mkWordRangeDec d (l,h) = mkWordRangeBase 10 quotRemPow10 pow10 len10 d l h
  where
    quotRemPow10 i x = x `quotRem` pow10 i
{-# INLINE mkWordRangeDec #-}

mkWordRangeHex
  :: (Word -> Word -> RE c Word)  -- Hexadecimal digit range
  -> (Word, Word)  -- Low high
  -> RE c Word
mkWordRangeHex d (l,h) = mkWordRangeBase 16 quotRemPow16 pow16 len16 d l h
  where
    quotRemPow16 i x = (x `unsafeShiftR` (4*i), x .&. (pow16 i - 1))
{-# INLINE mkWordRangeHex #-}

mkSignedIntRange
  :: RE c minus
  -> RE c plus
  -> ((Word, Word) -> RE c Word)  -- Word range
  -> (Int, Int)  -- Low high
  -> RE c Int
mkSignedIntRange minus plus wordRangeDec (low,high) = case (negR, nonNegR) of
  (Nothing, Nothing) -> empty
  (Nothing, Just r2) -> r2
  (Just r1, Nothing) -> r1
  (Just r1, Just r2) -> r1 <|> r2
  where
    negR
      | low > 0 = Nothing
      | otherwise = Just $
        minus *>
        R.fmap' (negate . fromIntegral)
                (wordRangeDec (absw (min 0 high), absw low))
    nonNegR
      | high < 0 = Nothing
      | otherwise = Just $
        (void plus <|> pure ()) *>
        R.fmap' fromIntegral
                (wordRangeDec (fromIntegral (max 0 low), fromIntegral high))
{-# INLINE mkSignedIntRange #-}

absw :: Int -> Word
absw = fromIntegral . abs

-------------------
-- Parsing ranges
-------------------

-- Make a tree based on the range. Keep the tree size small where possible.
-- This is hard to explain in words, so see here for some pictures:
-- https://github.com/meooow25/parser-regex/wiki/Visualizations#int-range

mkWordRangeBase
  :: forall c.
     Word  -- Base
  -> (Int -> Word -> (Word, Word)) -- quotRemPowBase
  -> (Int -> Word)  -- powBase
  -> (Word -> Int)  -- baseLen
  -> (Word -> Word -> RE c Word)  -- Decimal digit range
  -> Word  -- Low
  -> Word  -- High
  -> RE c Word
mkWordRangeBase _ _ _ _ _ low high | low > high = empty
mkWordRangeBase base quotRemPowBase powBase baseLen d low high
  = goTop (baseLen high - 1) True low high
  where
    goTop :: Int -> Bool -> Word -> Word -> RE c Word
    goTop 0 _ l h = d l h
    goTop i lz l h
      | dl == dh       = leading pBase dh dh (goTop (i-1) False l' h')
      | fullL && fullH = leading pBase dl dh (goFull (i-1))
      | fullH          = leading pBase (dl+1) dh (goFull (i-1)) <|> reL
      | fullL          = reH <|> leading pBase dl (dh-1) (goFull (i-1))
      | dl + 1 == dh   = reH <|> reL
      | otherwise      = reH <|> reM <|> reL
      where
        pBase = powBase i
        (dl,l') = quotRemPowBase i l
        (dh,h') = quotRemPowBase i h
        lz' = lz && dl == 0
        fullL = not lz' && l' == 0
        fullH = h' + 1 == pBase
        reL = if lz'
              then goL (i-1) True l'
              else leading pBase dl dl (goL (i-1) False l')
        reH = leading pBase dh dh (goH (i-1) h')
        reM = leading pBase (dl+1) (dh-1) (goFull (i-1))

    goL :: Int -> Bool -> Word -> RE c Word
    goL 0 _ l = d l (base-1)
    goL i lz l
      | not lz && l == 0 = goFull i
      | dl == base-1     = reL
      | otherwise        = reM <|> reL
      where
        pBase = powBase i
        (dl,l') = quotRemPowBase i l
        reL = if lz && dl == 0
              then goL (i-1) True l'
              else leading pBase dl dl (goL (i-1) False l')
        reM = leading pBase (dl+1) (base-1) (goFull (i-1))

    goH :: Int -> Word -> RE c Word
    goH 0 h = d 0 h
    goH i h
      | h + 1 == pBase * base = goFull i
      | dh == 0               = reH
      | otherwise             = reH <|> reM
      where
        pBase = powBase i
        (dh,h') = quotRemPowBase i h
        reH = leading pBase dh dh (goH (i-1) h')
        reM = leading pBase 0 (dh-1) (goFull (i-1))

    goFull :: Int -> RE c Word
    goFull 0 = d 0 (base-1)
    goFull i = leading (powBase i) 0 (base-1) (goFull (i-1))

    leading :: Word -> Word -> Word -> RE c Word -> RE c Word
    leading !pBase dl dh = R.liftA2' (\x y -> x * pBase + y) (d dl dh)
{-# INLINE mkWordRangeBase #-}

---------------------------------
-- Parsing hexadecimal Naturals
---------------------------------

-- Parsing hexadecimal is simple, there is no base conversion involved.
--
-- Step 1: Accumulate the hex digits, packed into Words
-- Step 2:
--   * GHC: Initialize a ByteArray and fill it with the Words. This takes
--     O(n) time. Because we create a Nat directly, this makes us depend on
--     ghc-bignum and GHC>=9.0.
--   * Not GHC: Do it like we do for decimal, without being aware of the
--     representation of Naturals, but replace the base multiplications with
--     shifts. If it is a binary representation, this takes O(n log n) time
--     instead of O(n^2).

stepHex :: NatParseState -> Word -> NatParseState
stepHex (NatParseState acc len ns) d
  | len < maxBoundWordHexLen = NatParseState (acc*16 + d) (len+1) ns
  | otherwise = NatParseState d 1 (WCons acc ns)

finishHex
  :: Word          -- ^ Leading digit
  -> NatParseState -- ^ Everything else
  -> Natural
#ifdef __GLASGOW_HASKELL__
finishHex !ld (NatParseState acc0 len0 ns0) = case ns0 of
  WNil -> Nat.naturalFromWord (ld `unsafeShiftL` (4*(len0-1)) + acc0)
  WCons n ns1 ->
    let lns = lengthWList ns1 + 2
        wsz = WORD_SIZE_IN_BITS
        !(PrimArray byteArray) = runPrimArray $ do
          ma <- newPrimArray lns
          if len0 == maxBoundWordHexLen
          then do
            let go i n1 WNil = do
                  let n1' = ld `unsafeShiftL` (4*(maxBoundWordHexLen-1)) + n1
                  writePrimArray ma i n1'
                go i n1 (WCons n2 ns2) = do
                  writePrimArray ma i n1
                  go (i+1) n2 ns2
            writePrimArray ma 0 acc0
            go 1 n ns1
          else do
            let go i prv n1 WNil = do
                  let n1' = ld `unsafeShiftL` (4*(maxBoundWordHexLen-1)) + n1
                  writePrimArray ma i (prv + n1' `unsafeShiftL` (4*len0))
                  writePrimArray ma (i+1) (n1' `unsafeShiftR` (wsz - 4*len0))
                go i prv n1 (WCons n2 ns2) = do
                  writePrimArray ma i (prv + n1 `unsafeShiftL` (4*len0))
                  go (i+1) (n1 `unsafeShiftR` (wsz - 4*len0)) n2 ns2
            go 0 acc0 n ns1
          pure ma
    in Nat.NB byteArray
-- finishHex does a bunch of unsafe stuff, so make sure things are correct:
-- * Bit shifts are in [0..wsz-1]
-- * Natural invariants:
--   * If the value fits in a word, it must be NS (via naturalFromWord here).
--   * Otherwise, use a ByteArray# with NB. The highest Word must not be 0.
#else
finishHex !ld (NatParseState acc0 len0 ns0) = combine acc0 len0 ns0
  where
    combine !acc !len ns = case ns of
      WNil -> mul16Pow (w2n ld) (len-1) + w2n acc
      WCons n ns1 ->
        mul16Pow (combine1 maxBoundWordHexLen (go n ns1)) len + w2n acc
      where
        go n WNil =
          let !n' = mul16Pow (w2n ld) (maxBoundWordHexLen - 1) + w2n n
          in [n']
        go n (WCons m WNil) =
          let !n' = mul16Pow (w2n ld) (2 * maxBoundWordHexLen - 1) +
                    mul16Pow (w2n m) maxBoundWordHexLen +
                    w2n n
          in [n']
        go n (WCons m (WCons n1 ns1)) =
          let !n' = mul16Pow (w2n m) maxBoundWordHexLen + w2n n
          in n' : go n1 ns1

    combine1 :: Int -> [Natural] -> Natural
    combine1 !_ [n] = n
    combine1 !numDigs ns1 = combine1 numDigs1 (go ns1)
      where
        numDigs1 = 2 * numDigs
        go (n:m:ns) = let !n' = mul16Pow m numDigs1 + n in n' : go ns
        go ns = ns

    mul16Pow :: Natural -> Int -> Natural
    mul16Pow x p = unsafeShiftL x (4 * p)
#endif

-----------------------------
-- Parsing decimal Naturals
-----------------------------

-- The implementation below is adapted from the bytestring package.
-- https://github.com/haskell/bytestring/blob/7e11412b9bfb13bcd6b8e7c04765b8f5bd90fd34/Data/ByteString/Lazy/ReadNat.hs
--
-- Step 1: Accumulate the digits, packed into Words.
-- Step 2: Combine the packed Words bottom-up into the result. This is what
--         makes it better than foldl (\acc d -> acc * 10 + d)).
--
-- The obvious foldl approach is O(n^2) for n digits. The combine approach
-- performs O(n/2^i) multiplications of size O(2^i), for i in [0..log_2(n)].
-- If multiplication is O(n^k), this is also O(n^k).
--
-- On GHC, we have k < 2, thanks to subquadratic multiplication of GMP-backed
-- Naturals: https://gmplib.org/manual/Multiplication-Algorithms.
--
-- For reference, here's how GMP converts any base (including 10) to a natural
-- using broadly the same approach.
-- https://github.com/alisw/GMP/blob/2bbd52703e5af82509773264bfbd20ff8464804f/mpn/generic/set_str.c

stepDec :: NatParseState -> Word -> NatParseState
stepDec (NatParseState acc len ns) d
  | len < safeWordDecLen = NatParseState (10*acc + d) (len+1) ns
  | otherwise = NatParseState d 1 (WCons acc ns)

finishDec
  :: Word          -- ^ Leading digit
  -> NatParseState -- ^ Everything else
  -> Natural
finishDec !ld (NatParseState acc0 len0 ns0) = combine acc0 len0 ns0
  where
    combine !acc !len ns = case ns of
      WNil -> w2n (10^(len-1) * ld + acc)
      WCons n ns1 -> 10^len * combine1 safeBaseDec (go n ns1) + w2n acc
      where
        go n WNil = let !n' = w2n (highMulDec * ld + n) in [n']
        go n (WCons m WNil) =
          let !n' = w2n (highMulDec * ld + m) * safeBaseDec + w2n n in [n']
        go n (WCons m (WCons n1 ns1)) =
          let !n' = w2n m * safeBaseDec + w2n n in n' : go n1 ns1

    combine1 :: Natural -> [Natural] -> Natural
    combine1 _ [n] = n
    combine1 base ns1 = combine1 base1 (go ns1)
      where
        !base1 = base * base
        go (n:m:ns) = let !n' = m * base1 + n in n' : go ns
        go ns = ns

w2n :: Word -> Natural
w2n = fromIntegral

safeBaseDec :: Natural
safeBaseDec = fromIntegral (pow10 safeWordDecLen)

highMulDec :: Word
highMulDec = pow10 (safeWordDecLen - 1)

---------------------------
-- Common Natural parsing
---------------------------

data WList = WCons {-# UNPACK #-} !Word !WList | WNil

data NatParseState = NatParseState
  {-# UNPACK #-} !Word      -- ^ acc
  {-# UNPACK #-} !Int       -- ^ length of acc in some base
                 !WList     -- ^ accs, little endian

lengthWList :: WList -> Int
lengthWList = go 0
  where
    go !acc WNil = acc
    go acc (WCons _ ns) = go (acc+1) ns

--------------------
-- Low level stuff
--------------------

-- | Length in base 16.
len16 :: Word -> Int
len16 0 = 1
len16 x = maxBoundWordHexLen - (countLeadingZeros x `div` 4)

-- | 16^i. i must not be large enough to overflow a Word.
pow16 :: Int -> Word
pow16 i = 1 `unsafeShiftL` (4*i)

-- | Length in base 10.
len10 :: Word -> Int
len10 x = go 1 1
  where
    x' = x `quot` 10
    go p i | x' < p = i
    go p i = go (p*10) (i+1)

-- | "999..." repeated safeWordDecLen times is guaranteed to fit in a Word.
safeWordDecLen :: Int

-- | Decimal length of (maxBound :: Word)
maxBoundWordDecLen :: Int

-- | Hexadecimal length of (maxBound :: Word)
maxBoundWordHexLen :: Int

-- | 10^i. i must not be large enough to overflow a Word.
pow10 :: Int -> Word

#if WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64

#if WORD_SIZE_IN_BITS == 64
safeWordDecLen = 19
maxBoundWordDecLen = 20
maxBoundWordHexLen = 16
#else
safeWordDecLen = 9
maxBoundWordDecLen = 10
maxBoundWordHexLen = 8
#endif

pow10 p = case p of
  0 -> 1
  1 -> 10
  2 -> 100
  3 -> 1000
  4 -> 10000
  5 -> 100000
  6 -> 1000000
  7 -> 10000000
  8 -> 100000000
  9 -> 1000000000
#if WORD_SIZE_IN_BITS == 64
  10 -> 10000000000
  11 -> 100000000000
  12 -> 1000000000000
  13 -> 10000000000000
  14 -> 100000000000000
  15 -> 1000000000000000
  16 -> 10000000000000000
  17 -> 100000000000000000
  18 -> 1000000000000000000
  19 -> 10000000000000000000
#endif
  _ -> error "Regex.Internal.Int.pow10: p too large"
#else
#error "unsupported word size"
#endif
