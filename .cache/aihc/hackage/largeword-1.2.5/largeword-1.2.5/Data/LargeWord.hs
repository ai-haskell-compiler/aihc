{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

{-# LANGUAGE CPP                           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LargeWord
-- Copyright   :  (c) Dominic Steinitz 2004 - 2014
-- License     :  BSD
--
-- Maintainer  :  dominic@steinitz.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides Word128, Word192 and Word256 and a way of producing other
-- large words if required.
--
-----------------------------------------------------------------------------

module Data.LargeWord
  ( LargeKey(..)
  , Word96
  , Word128
  , Word160
  , Word192
  , Word224
  , Word256
  , loHalf
  , hiHalf
  ) where

import Data.Word
import Data.Bits
import Numeric

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)


#if !(MIN_VERSION_base(4,7,0))
class FiniteBits a where
  finiteBitSize :: a -> Int

instance FiniteBits Word8 where
  finiteBitSize = bitSize

instance FiniteBits Word16 where
  finiteBitSize = bitSize

instance FiniteBits Word32 where
  finiteBitSize = bitSize

instance FiniteBits Word64 where
  finiteBitSize = bitSize
#endif

-- Keys have certain capabilities.

class LargeWord a where
   largeWordToInteger :: a -> Integer
   integerToLargeWord :: Integer -> a
   largeWordPlus      :: a -> a -> a
   largeWordMinus     :: a -> a -> a
   largeWordAnd       :: a -> a -> a
   largeWordOr        :: a -> a -> a
   largeWordShift     :: a -> Int -> a
   largeWordXor       :: a -> a -> a
   largeBitSize       :: a -> Int

-- Word8 is a key in the obvious way

instance LargeWord Word8 where
  largeWordToInteger = toInteger
  integerToLargeWord = fromInteger
  largeWordPlus      = (+)
  largeWordMinus     = (-)
  largeWordAnd       = (.&.)
  largeWordOr        = (.|.)
  largeWordShift     = shift
  largeWordXor       = xor
  largeBitSize       = finiteBitSize

-- Word16 is a key in the obvious way

instance LargeWord Word16 where
  largeWordToInteger = toInteger
  integerToLargeWord = fromInteger
  largeWordPlus      = (+)
  largeWordMinus     = (-)
  largeWordAnd       = (.&.)
  largeWordOr        = (.|.)
  largeWordShift     = shift
  largeWordXor       = xor
  largeBitSize       = finiteBitSize

-- Word32 is a key in the obvious way.

instance LargeWord Word32 where
  largeWordToInteger = toInteger
  integerToLargeWord = fromInteger
  largeWordPlus      = (+)
  largeWordMinus     = (-)
  largeWordAnd       = (.&.)
  largeWordOr        = (.|.)
  largeWordShift     = shift
  largeWordXor       = xor
  largeBitSize       = finiteBitSize

-- Word64 is a key in the obvious way.

instance LargeWord Word64 where
  largeWordToInteger = toInteger
  integerToLargeWord = fromInteger
  largeWordPlus      = (+)
  largeWordMinus     = (-)
  largeWordAnd       = (.&.)
  largeWordOr        = (.|.)
  largeWordShift     = shift
  largeWordXor       = xor
  largeBitSize       = finiteBitSize

-- Define larger keys from smaller ones.

data LargeKey a b = LargeKey a b
   deriving (Eq)

{-# INLINE loHalf #-}
loHalf :: LargeKey a b -> a
loHalf (LargeKey a _b) = a
{-# INLINE hiHalf #-}
hiHalf :: LargeKey a b -> b
hiHalf (LargeKey _a b) = b

instance (Ord a, Bits a, FiniteBits a, Num a, LargeWord a, Bits b, FiniteBits b, Num b, LargeWord b) =>
   LargeWord (LargeKey a b) where
      largeWordToInteger (LargeKey lo hi) =
         largeWordToInteger lo + (2^(finiteBitSize lo)) * largeWordToInteger hi
      integerToLargeWord x =
         let (h,l) =  x `quotRem` (2^(finiteBitSize lo))
             (lo,hi) = (integerToLargeWord l, integerToLargeWord h) in
                LargeKey lo hi
      largeWordPlus (LargeKey alo ahi) (LargeKey blo bhi) =
         LargeKey lo' hi' where
            lo' = alo + blo
            hi' = ahi + bhi + if lo' < alo then 1 else 0
      largeWordMinus (LargeKey alo ahi) (LargeKey blo bhi) =
         LargeKey lo' hi' where
            lo' = alo - blo
            hi' = ahi - bhi - if lo' > alo then 1 else 0
      largeWordAnd (LargeKey alo ahi) (LargeKey blo bhi) =
         LargeKey lo' hi' where
            lo' = alo .&. blo
            hi' = ahi .&. bhi
      largeWordOr (LargeKey alo ahi) (LargeKey blo bhi) =
         LargeKey lo' hi' where
            lo' = alo .|. blo
            hi' = ahi .|. bhi
      largeWordXor (LargeKey alo ahi) (LargeKey blo bhi) =
         LargeKey lo' hi' where
            lo' = alo `xor` blo
            hi' = ahi `xor` bhi
      largeWordShift w 0 = w
      largeWordShift (LargeKey lo hi) x =
         if x >= 0
         then
           if loSize <= hiSize
           then
             LargeKey (shift lo x)
                      (shift hi x .|. (shift (convab lo) (x - (finiteBitSize lo))))
           else
             LargeKey (shift lo x)
                      (shift hi x .|. (convab (shift lo (x - (finiteBitSize lo)))))
         else
           if loSize <= hiSize
           then
             LargeKey (shift lo x .|. (convba (shift hi (x + (finiteBitSize lo)))))
                      (shift hi x)
           else
             LargeKey (shift lo x .|. (shift (convba hi) (x + (finiteBitSize lo))))
                      (shift hi x)
         where
           loSize = finiteBitSize lo
           hiSize = finiteBitSize hi
           convab = integerToLargeWord . largeWordToInteger
           convba = integerToLargeWord . largeWordToInteger
      largeBitSize ~(LargeKey lo hi) = largeBitSize lo + largeBitSize hi

instance (Ord a, Bits a, FiniteBits a, Num a, LargeWord a, Bits b, FiniteBits b, Num b, LargeWord b) => Show (LargeKey a b) where
   showsPrec _p = showInt . largeWordToInteger

instance (Ord b, Ord a, Bits a, FiniteBits a, Num a, LargeWord a, Bits b, FiniteBits b, Num b, LargeWord b) =>
   Num (LargeKey a b) where
      (+) = largeWordPlus
      (-) = largeWordMinus
      (*) a b =  go 0 0
        where
        go i r
         | i == finiteBitSize r = r
         | testBit b i = go (i+1) (r + (a `shiftL` i))
         | otherwise   = go (i+1) r
      negate = id
      abs    = id
      signum a = if a > 0 then 1 else 0
      fromInteger = integerToLargeWord

-- Larger keys are instances of Bits provided their constituents are keys.

instance (Ord a, Ord b, Bits a, FiniteBits a, Num a, LargeWord a, Bits b, FiniteBits b, Num b, LargeWord b) =>
   Bits (LargeKey a b) where
      (.&.) = largeWordAnd
      (.|.) = largeWordOr
      xor = largeWordXor
      shift = largeWordShift
      x `rotate`  i | i < 0  = (x `largeWordShift` i) .|.
                               (x `largeWordShift` (i + largeBitSize x))
                    | i == 0 = x
                    | i > 0  = (x `largeWordShift` i) .|.
                               (x `largeWordShift` (i - largeBitSize x))
                    | otherwise = error $ "Clearly i must be < 0, == 0 or > 0" ++
                                          "but ghc can't determine this"
      complement (LargeKey a b) = LargeKey (complement a) (complement b)
      bitSize = largeBitSize
#if MIN_VERSION_base(4,7,0)
      bitSizeMaybe = Just . largeBitSize
#endif
      isSigned _ = False
#if MIN_VERSION_base(4,6,0)
      bit = bitDefault
      testBit = testBitDefault
      popCount = popCountDefault
#endif

instance (LargeWord a, Bits a, FiniteBits a, Ord a, Num a,
          LargeWord b, Bits b, FiniteBits b, Ord b, Num b) => FiniteBits (LargeKey a b) where
  finiteBitSize = largeBitSize

instance (Ord a, Bits a, FiniteBits a, Bounded a, Integral a, LargeWord a,
                 Bits b, FiniteBits b, Bounded b, Integral b, LargeWord b) =>
   Bounded (LargeKey a b) where
      minBound = 0
      maxBound =
         result where
            result =
               fromIntegral $
               (1 + fromIntegral (maxBound `asTypeOf` (boflk result)))*
                  (1 + fromIntegral (maxBound `asTypeOf` (aoflk result))) - 1

aoflk :: (LargeKey a b) -> a
aoflk = undefined
boflk :: (LargeKey a b) -> b
boflk = undefined

instance (Bounded a, Bounded b, Enum b, Enum a, Ord a, Bits a, FiniteBits a, Num a, LargeWord a, Ord b, Bits b, FiniteBits b, Num b, LargeWord b) =>
   Integral (LargeKey a b) where
      toInteger = largeWordToInteger
      quotRem a b =
              let r = a - q*b
                  q = go 0 (finiteBitSize a) 0
              in (q,r)
       where
       -- Trivial long division
       go t 0 v = if v >= b then t+1 else t
       go t i v
              | v >= b    = go (setBit t i) i' v2
              | otherwise = go t i' v1
         where i' = i - 1
               newBit = if (testBit a i') then 1 else 0
               v1 = (v `shiftL` 1) .|. newBit
               v2 = ((v - b) `shiftL` 1) .|. newBit
      divMod = quotRem

instance (Ord a, Bits a, FiniteBits a, Num a, Bounded a, Bounded b, Enum a, Enum b, LargeWord a, Ord b, Bits b, FiniteBits b, Num b, LargeWord b) => Real (LargeKey a b) where
      toRational w = toRational (fromIntegral w :: Integer)


instance (Eq a, Bounded a, Num a, Enum b, Enum a, Bounded b, Num b) => Enum (LargeKey a b) where
	toEnum i = LargeKey (toEnum i) 0
	fromEnum (LargeKey l _) = fromEnum l
	pred (LargeKey 0 h) = LargeKey maxBound (pred h)
	pred (LargeKey l h) = LargeKey (pred l) h
	succ (LargeKey l h) = if l == maxBound then LargeKey 0 (succ h)
                                               else LargeKey (succ l) h

instance (Binary a, Binary b) => Binary (LargeKey a b) where
   put (LargeKey lo hi) = put hi >> put lo
   get = flip LargeKey <$> get <*> get

instance (Ord a, Ord b) => Ord (LargeKey a b) where
    compare a b = compare (hiHalf a, loHalf a) (hiHalf b, loHalf b)

type Word96  = LargeKey Word32 Word64
type Word128 = LargeKey Word64 Word64
type Word160 = LargeKey Word32 Word128
type Word192 = LargeKey Word64 Word128
type Word224 = LargeKey Word32 Word192
type Word256 = LargeKey Word64 Word192
