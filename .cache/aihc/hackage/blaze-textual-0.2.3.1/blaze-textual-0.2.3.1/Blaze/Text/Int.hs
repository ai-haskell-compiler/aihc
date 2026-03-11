{-# LANGUAGE BangPatterns, CPP, MagicHash, OverloadedStrings, UnboxedTuples #-}

-- Module:      Blaze.Text.Int
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize an integral value as a lazy 'L.ByteString'.

module Blaze.Text.Int
    (
      digit
    , integral
    , minus
    ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Data.ByteString.Char8 ()
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (mappend, mempty)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Base (quotInt, remInt)
#if MIN_VERSION_base(4,15,0)
#else
import GHC.Num (quotRemInteger)
#endif
import GHC.Types (Int(..))

#if defined(INTEGER_GMP)
import GHC.Integer.GMP.Internals
#elif defined(INTEGER_SIMPLE)
import GHC.Integer.Simple.Internals
#endif

#define PAIR(a,b) (# a,b #)

integral :: (Integral a, Show a) => a -> Builder
{-# RULES "integral/Int" integral = bounded :: Int -> Builder #-}
{-# RULES "integral/Int8" integral = bounded :: Int8 -> Builder #-}
{-# RULES "integral/Int16" integral = bounded :: Int16 -> Builder #-}
{-# RULES "integral/Int32" integral = bounded :: Int32 -> Builder #-}
{-# RULES "integral/Int64" integral = bounded :: Int64 -> Builder #-}
{-# RULES "integral/Word" integral = nonNegative :: Word -> Builder #-}
{-# RULES "integral/Word8" integral = nonNegative :: Word8 -> Builder #-}
{-# RULES "integral/Word16" integral = nonNegative :: Word16 -> Builder #-}
{-# RULES "integral/Word32" integral = nonNegative :: Word32 -> Builder #-}
{-# RULES "integral/Word64" integral = nonNegative :: Word64 -> Builder #-}
{-# RULES "integral/Integer" integral = integer :: Integer -> Builder #-}

-- This definition of the function is here PURELY to be used by ghci
-- and those rare cases where GHC is being invoked without
-- optimization, as otherwise the rewrite rules above should fire. The
-- test for `(-i) == i` catches when we render minBound, in which case
-- using `-i` would be wrong. An example is `-(-128 :: Int8) == -128`.
integral i
    | i >= 0                 = nonNegative i
    | (-i) == i              = fromString (show i)
    | otherwise              = b
  where b = minus `mappend` nonNegative (-i)
{-# NOINLINE integral #-}

bounded :: (Bounded a, Integral a) => a -> Builder
{-# SPECIALIZE bounded :: Int -> Builder #-}
{-# SPECIALIZE bounded :: Int8 -> Builder #-}
{-# SPECIALIZE bounded :: Int16 -> Builder #-}
{-# SPECIALIZE bounded :: Int32 -> Builder #-}
{-# SPECIALIZE bounded :: Int64 -> Builder #-}
bounded i
    | i >= 0        = nonNegative i
    | i > minBound  = minus `mappend` nonNegative (-i)
    | otherwise     = minus `mappend`
                      nonNegative (negate (k `quot` 10)) `mappend`
                      digit (negate (k `rem` 10))
  where k = minBound `asTypeOf` i

nonNegative :: Integral a => a -> Builder
{-# SPECIALIZE nonNegative :: Int -> Builder #-}
{-# SPECIALIZE nonNegative :: Int8 -> Builder #-}
{-# SPECIALIZE nonNegative :: Int16 -> Builder #-}
{-# SPECIALIZE nonNegative :: Int32 -> Builder #-}
{-# SPECIALIZE nonNegative :: Int64 -> Builder #-}
{-# SPECIALIZE nonNegative :: Word -> Builder #-}
{-# SPECIALIZE nonNegative :: Word8 -> Builder #-}
{-# SPECIALIZE nonNegative :: Word16 -> Builder #-}
{-# SPECIALIZE nonNegative :: Word32 -> Builder #-}
{-# SPECIALIZE nonNegative :: Word64 -> Builder #-}
nonNegative x
  | x < 0 = error $ "nonNegative: Called with negative number " ++ show (fromIntegral x :: Integer)
  | otherwise = go x
  where
    go n | n < 10    = digit n
         | otherwise = go (n `quot` 10) `mappend` digit (n `rem` 10)

digit :: Integral a => a -> Builder
digit n = fromWord8 $! fromIntegral n + 48
{-# INLINE digit #-}

minus :: Builder
minus = fromWord8 45

int :: Int -> Builder
int = integral
{-# INLINE int #-}

integer :: Integer -> Builder
#if defined(INTEGER_GMP)
integer (S# i#) = int (I# i#)
#endif
integer i
    | i < 0     = minus `mappend` go (-i)
    | otherwise = go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf p n
      | p > n       = [n]
      | otherwise   = splith p (splitf (p*p) n)

    splith p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) | q > 0     -> q : r : splitb p ns
                                  | otherwise -> r : splitb p ns
    splith _ _      = error "splith: the impossible happened."

    splitb p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) -> q : r : splitb p ns
    splitb _ _      = []

data T = T !Integer !Int

fstT :: T -> Integer
fstT (T a _) = a

maxInt :: Integer
maxDigits :: Int
T maxInt maxDigits =
    until ((>mi) . (*10) . fstT) (\(T n d) -> T (n*10) (d+1)) (T 10 1)
  where mi = fromIntegral (maxBound :: Int)

putH :: [Integer] -> Builder
putH (n:ns) = case n `quotRemInteger` maxInt of
                PAIR(x,y)
                    | q > 0     -> int q `mappend` pblock r `mappend` putB ns
                    | otherwise -> int r `mappend` putB ns
                    where q = fromInteger x
                          r = fromInteger y
putH _ = error "putH: the impossible happened"

putB :: [Integer] -> Builder
putB (n:ns) = case n `quotRemInteger` maxInt of
                PAIR(x,y) -> pblock q `mappend` pblock r `mappend` putB ns
                    where q = fromInteger x
                          r = fromInteger y
putB _ = mempty

pblock :: Int -> Builder
pblock = go maxDigits
  where
    go !d !n
        | d == 1    = digit n
        | otherwise = go (d-1) q `mappend` digit r
        where q = n `quotInt` 10
              r = n `remInt` 10
