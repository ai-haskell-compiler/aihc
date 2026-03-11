-- |
-- Module:          Text.FShow.RealFloat.Internals
-- Copyright:       (c) 2011 Daniel Fischer
-- Licence:         BSD3
-- Maintainer:      Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:       experimental
-- Portability:     non-portable (GHC extensions)
--
-- Faster digit string generation for floating point numbers.
-- Uses a modification of the Integer showing code from "GHC.Num".
{-# LANGUAGE CPP, BangPatterns, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.FShow.RealFloat.Internals
    ( posToDigits
    , i2D
    , integerLog2
    ) where

#include "MachDeps.h"

import GHC.Base
#if __GLASGOW_HASKELL__ < 705
import GHC.Num (quotRemInt)
#endif
import GHC.Integer
import Data.Array.Base (unsafeAt)
import Data.Array.IArray

#if __GLASGOW_HASKELL__ >= 702
import GHC.Integer.Logarithms

-- | Integer base-@2@ logarithm of a positive 'Integer'.
{-# INLINE integerLog2 #-}
integerLog2 :: Integer -> Int
integerLog2 n = I# (integerLog2# n)
#else
import GHC.Float (integerLogBase)

-- | Integer base-@2@ logarithm of a positive 'Integer'.
{-# INLINE integerLog2 #-}
integerLog2 :: Integer -> Int
integerLog2 = integerLogBase 2
#endif

#if __GLASGOW_HASKELL__ < 707
isTrue# :: Bool -> Bool
isTrue# = id
#endif

#if WORD_SIZE_IN_BITS == 32
#define DIGITS       9
#define BASE         1000000000
#else
#define DIGITS       18
#define BASE         1000000000000000000
#endif

-- unsafe Int -> Char conversion for decimal digits
{-# INLINE i2D #-}
i2D :: Int -> Char
i2D (I# i#) = C# (chr# (ord# '0'# +# i#))

-- | 'posToDigits' converts a positive number into a list of digits and
--   an exponent. If @x = 10^e*d_1.d_2...d_m...@ with @d_1 /= 0@ and
--   @0 <= d_i <= 9@, the result is @([d_1,d_2,...,d_m],e)@, where
--   @m@ is one or two larger than the number of requested digits,
--   provided that @2^(-70776) <= x < 2^248236@ (with 64-bit 'Int's,
--   the upper bound is about @2^1.3e9@).
--
--   The number @x@ is (indirectly) given in the form
--   @mantissa * 2^exponent@, similar to 'encodeFloat',
--   as the final two arguments. The second argument is the base-2
--   logarithm of the mantissa and the first is the number of decimal
--   digits needed to discriminate between different numbers.
--
--   In @'posToDigits' digs mlog mant exp@, it is assumed that
--
-- * @digs > 0@, @mlog >= 0@,
--
-- * @2^mlog <= mant < 2^(mlog+1)@.
--
--   These assumptions are not checked, and if they're not satisfied,
--   wrong results or worse are the consequences. /You have been warned/.
--
--   The digits argument may be smaller than would be necessary to uniquely
--   determine each value if that is not required. As a rule of thumb,
--   requiring fewer significant digits means faster generation of the
--   representation.
{-# INLINE posToDigits #-}
posToDigits :: Int      -- ^ number of digits required
            -> Int      -- ^ base @2@ logarithm of the mantissa
            -> Integer  -- ^ mantissa
            -> Int      -- ^ scaling exponent
            -> ([Int], Int)
posToDigits showDigs mantExp mant scaleExp@(I# e#) = (integerToDigits decMant, e10)
  where
    !rex = mantExp + scaleExp
    !l0 = (8651*rex) `quot` 28738
    !l10 = if rex < 0 then l0-1 else l0
    -- 10^l10 <= x < 10^(l10+2)
    !decshift@(I# d#) = showDigs - l10
    !binshift = e# +# d#
    !decMant
        | isTrue# (d# <# 0#) =
            (if isTrue# (binshift <# 0#)
                then shiftRInteger mant (negateInt# binshift)
                else shiftLInteger mant binshift) `quot` expt5 (I# (negateInt# d#))
        | isTrue# (binshift <# 0#) =
            shiftRInteger (mant * expt5 decshift) (negateInt# binshift)
        | otherwise = shiftLInteger (mant * expt5 decshift) binshift
    !e10 = if decMant < expt10 (showDigs+1) then l10 else l10+1

expt5 :: Int -> Integer
expt5 k = if k <= maxEx5 && k >= 0 then unsafeAt expts5 k else 5^k

expt10 :: Int -> Integer
expt10 k = if k <= maxEx10 && k >= 0 then unsafeAt expts10 k else 10^k

maxEx5 :: Int
maxEx5 = 349

maxEx10 :: Int
maxEx10 = 25

expts5 :: Array Int Integer
expts5 = array (0, maxEx5) [(k,5^k) | k <- [0 .. maxEx5]]

expts10 :: Array Int Integer
expts10 = array (0,maxEx10) [(k,10^k) | k <- [0 .. maxEx10]]

------------------------------------------------------------------------------
--  The code to show Integers, modified to produce [Int] instead of [Char]
--  Taken from GHC.Num and modified to suit our needs
--  The GHC Licence is reproduced in the package root

-- Divide and conquer implementation
-- generate the sequence of digits of a positive Integer
integerToDigits :: Integer -> [Int]
integerToDigits nm
    | nm < BASE = jhead (fromInteger nm) []
    | otherwise = jprinth (jsplitf (BASE*BASE) nm) []
      where

        -- Split n into digits in base p. We first split n into digits
        -- in base p*p and then split each of these digits into two.
        -- Note that the first 'digit' modulo p*p may have a leading zero
        -- in base p that we need to drop - this is what jsplith takes care of.
        -- jsplitb the handles the remaining digits.
        jsplitf :: Integer -> Integer -> [Integer]
        jsplitf p n
            | p > n     = [n]
            | otherwise = jsplith p (jsplitf (p*p) n)

        jsplith :: Integer -> [Integer] -> [Integer]
        jsplith p (n:ns) =
            case n `quotRemInteger` p of
            (# q, r #) ->
                if q > 0 then q : r : jsplitb p ns
                        else     r : jsplitb p ns
        jsplith _ [] = error "jsplith: []"

        jsplitb :: Integer -> [Integer] -> [Integer]
        jsplitb _ []     = []
        jsplitb p (n:ns) = case n `quotRemInteger` p of
                        (# q, r #) ->
                            q : r : jsplitb p ns

        -- Convert a number that has been split into digits in base BASE^2
        -- this includes a last splitting step and then conversion of digits
        -- that all fit into a machine word.
        jprinth :: [Integer] -> [Int] -> [Int]
        jprinth (n:ns) cs =
            case n `quotRemInteger` BASE of
            (# q', r' #) ->
                let q = fromInteger q'
                    r = fromInteger r'
                in if q > 0 then jhead q $ jblock r $ jprintb ns cs
                            else jhead r $ jprintb ns cs
        jprinth [] _ = error "jprinth []"

        jprintb :: [Integer] -> [Int] -> [Int]
        jprintb []     cs = cs
        jprintb (n:ns) cs = case n `quotRemInteger` BASE of
                            (# q', r' #) ->
                                let q = fromInteger q'
                                    r = fromInteger r'
                                in jblock q $ jblock r $ jprintb ns cs

        -- Convert an integer that fits into a machine word. Again, we have two
        -- functions, one that drops leading zeros (jhead) and one that doesn't
        -- (jblock)
        jhead :: Int -> [Int] -> [Int]
        jhead n cs
            | n < 10    = n:cs
            | otherwise = jhead q (r : cs)
            where
            (q, r) = n `quotRemInt` 10

        jblock = jblock' {- ' -} DIGITS     -- bloody CPP

        jblock' :: Int -> Int -> [Int] -> [Int]
        jblock' d n cs
            | d == 1    = n : cs
            | otherwise = jblock' (d - 1) q (r : cs)
            where
            (q, r) = n `quotRemInt` 10
