{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE BangPatterns #-}
----------------------------------------------------------------
--                                                    2024-04-11
-- |
-- Module      :  Data.ByteString.Lex.Internal
-- Copyright   :  Copyright (c) 2010--2024 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  BangPatterns
--
-- Some functions we want to share across the other modules without
-- actually exposing them to the user.
----------------------------------------------------------------
module Data.ByteString.Lex.Internal
    (
    -- * Character-based bit-bashing
      isNotPeriod
    , isNotE
    , isDecimal
    , isDecimalZero
    , toDigit
    , addDigit
    -- * Integral logarithms
    , numDigits
    , numTwoPowerDigits
    , numDecimalDigits
    ) where

import Data.Word (Word8, Word64)
import Data.Bits (Bits(shiftR))

----------------------------------------------------------------
----------------------------------------------------------------
----- Character-based bit-bashing

{-# INLINE isNotPeriod #-}
isNotPeriod :: Word8 -> Bool
isNotPeriod w = w /= 0x2E

{-# INLINE isNotE #-}
isNotE :: Word8 -> Bool
isNotE w = w /= 0x65 && w /= 0x45

{-# INLINE isDecimal #-}
isDecimal :: Word8 -> Bool
isDecimal w = 0x39 >= w && w >= 0x30

{-# INLINE isDecimalZero #-}
isDecimalZero :: Word8 -> Bool
isDecimalZero w = w == 0x30

{-# INLINE toDigit #-}
toDigit :: (Integral a) => Word8 -> a
toDigit w = fromIntegral (w - 0x30)

{-# INLINE addDigit #-}
addDigit :: Int -> Word8 -> Int
addDigit n w = n * 10 + toDigit w

----------------------------------------------------------------
----- Integral logarithms

-- TODO: cf. integer-gmp:GHC.Integer.Logarithms made available in
-- version 0.3.0.0 (ships with GHC 7.2.1).
-- <http://haskell.org/ghc/docs/7.2.1/html/libraries/integer-gmp-0.3.0.0/GHC-Integer-Logarithms.html>


-- This implementation is derived from
-- <http://www.haskell.org/pipermail/haskell-cafe/2009-August/065854.html>
-- modified to use 'quot' instead of 'div', to ensure strictness,
-- and using more guard notation (but this last one's compiled
-- away). See @./bench/BenchNumDigits.hs@ for other implementation
-- choices.
--
-- | @numDigits b n@ computes the number of base-@b@ digits required
-- to represent the number @n@. N.B., this implementation is unsafe
-- and will throw errors if the base is @(<= 1)@, or if the number
-- is negative. If the base happens to be a power of 2, then see
-- 'numTwoPowerDigits' for a more efficient implementation.
--
-- We must be careful about the input types here. When using small
-- unsigned types or very large values, the repeated squaring can
-- overflow causing the function to loop. (E.g., the fourth squaring
-- of 10 overflows 32-bits (==1874919424) which is greater than the
-- third squaring. For 64-bit, the 5th squaring overflows, but it's
-- negative so will be caught.) Forcing the type to Integer ensures
-- correct behavior, but makes it substantially slower.

numDigits :: Integer -> Integer -> Int
{-# INLINE numDigits #-}
numDigits !b0 !n0
    | b0 <= 1   = error (_numDigits ++ _nonpositiveBase)
    | n0 <  0   = error (_numDigits ++ _negativeNumber)
    -- BUG: need to check n0 to be sure we won't overflow Int
    | otherwise = finish (ilog b0 n0)
    where
    finish (ND e _) = 1 + e
    ilog !b !n
        | n < b     = ND 0 n
        -- TODO(2024-04-11): Check core to see whether these @(2*)@
        -- ops are properly weakened to shifts.
        | r < b     = ND (2*e) r
        | otherwise = ND (2*e+1) (r `quot` b)
        where
        -- TODO(2024-04-11): Benchmark this lazy-pattern matching,
        -- vs using a strict pattern (and alas less guard-notation,
        -- to ensure we only evaluate it when needed).
        ND e r = ilog (b*b) n

-- TODO(2024-04-11): Benchmark this change in the implementation
-- (relative to using @(,)@ and @($!)@).  Also, need to re-run all
-- the benchmarks anyways, to see how things've changed on newer GHC.
data ND = ND {-#UNPACK#-}!Int !Integer


-- | Compute the number of base-@2^p@ digits required to represent a
-- number @n@. N.B., this implementation is unsafe and will throw
-- errors if the base power is non-positive, or if the number is
-- negative. For bases which are not a power of 2, see 'numDigits'
-- for a more general implementation.
numTwoPowerDigits :: (Integral a, Bits a) => Int -> a -> Int
{-# INLINE numTwoPowerDigits #-}
numTwoPowerDigits !p !n0
    | p  <= 0   = error (_numTwoPowerDigits ++ _nonpositiveBase)
    | n0 <  0   = error (_numTwoPowerDigits ++ _negativeNumber)
    | n0 == 0   = 1
    -- BUG: need to check n0 to be sure we won't overflow Int
    | otherwise = go 0 n0
    where
    go !d !n
        | n > 0     = go (d+1) (n `shiftR` p)
        | otherwise = d


-- This implementation is from:
-- <http://www.serpentine.com/blog/2013/03/20/whats-good-for-c-is-good-for-haskell/>
--
-- | Compute the number of base-@10@ digits required to represent
-- a number @n@. N.B., this implementation is unsafe and will throw
-- errors if the number is negative.
numDecimalDigits :: (Integral a) => a -> Int
{-# INLINE numDecimalDigits #-}
numDecimalDigits n0
    | n0 < 0     = error (_numDecimalDigits ++ _negativeNumber)
    -- Unfortunately this causes significant (1.2x) slowdown since
    -- GHC can't see it will always fail for types other than Integer...
    -- TODO(2024-04-11): See if we can't do more static-analysis
    -- code to optimize this path (a~la my C++ safe comparisons)
    | n0 > limit = numDigits 10 (toInteger n0)
    | otherwise  = go 1 (fromIntegral n0 :: Word64)
    where
    limit = fromIntegral (maxBound :: Word64)

    fin n bound = if n >= bound then 1 else 0
    go !k !n
        | n < 10        = k
        | n < 100       = k + 1
        | n < 1000      = k + 2
        | n < 1000000000000 =
            k + if n < 100000000
                then if n < 1000000
                    then if n < 10000
                        then 3
                        else 4 + fin n 100000
                    else 6 + fin n 10000000
                else if n < 10000000000
                    then 8 + fin n 1000000000
                    else 10 + fin n 100000000000
        | otherwise = go (k + 12) (n `quot` 1000000000000)


_numDigits :: String
_numDigits = "numDigits"
{-# NOINLINE _numDigits #-}

_numTwoPowerDigits :: String
_numTwoPowerDigits = "numTwoPowerDigits"
{-# NOINLINE _numTwoPowerDigits #-}

_numDecimalDigits :: String
_numDecimalDigits = "numDecimalDigits"
{-# NOINLINE _numDecimalDigits #-}

_nonpositiveBase :: String
_nonpositiveBase = ": base must be greater than one"
{-# NOINLINE _nonpositiveBase #-}

_negativeNumber :: String
_negativeNumber = ": number must be non-negative"
{-# NOINLINE _negativeNumber #-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
