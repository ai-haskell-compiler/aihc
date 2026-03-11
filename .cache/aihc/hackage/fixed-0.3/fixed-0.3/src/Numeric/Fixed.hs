{-# LANGUAGE CApiFFI, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014-15 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Fixed precision arithmetic. This format is the same format used by
-- OpenGL ES 1's @GLfixed@ data type:
--
-- One sign bit, 15 bits to the left of the decimal place and 16 bits
-- to the right packed into a 32-bit integer.
-----------------------------------------------------------------------------
module Numeric.Fixed
  ( Fixed(..)
  , fromFixed
  , toFixed
  ) where

import Data.Bits
import Data.Coerce
import Data.Int
import Data.Ratio
import Data.Typeable
import Foreign.Storable
import Foreign.C.Types

-- | A signed 2s complement 15.16 scale fixed precision number
newtype {-# CTYPE "signed int" #-} Fixed = Fixed { getFixed :: CInt } deriving (Eq,Ord,Typeable,Storable)

-- | Convert from a 'Fixed' precision value to a 'Double'
fromFixed :: Fixed -> Double
fromFixed (Fixed x) = fromIntegral x / 65536

-- | Convert from a 'Double' to a 'Fixed' precision value
toFixed :: Double -> Fixed
toFixed x = Fixed $ floor (x * 65536 + 0.5)

instance Show Fixed where
  showsPrec d = showsPrec d . fromFixed

instance Num Fixed where
  (+) = coerce ((+) :: CInt -> CInt -> CInt)
  (-) = coerce ((-) :: CInt -> CInt -> CInt)
  negate = coerce (negate :: CInt -> CInt)
  abs = coerce (abs :: CInt -> CInt)
  signum (Fixed a) = Fixed $ unsafeShiftL (signum a) 16
  Fixed a * Fixed b = Fixed $ fromIntegral (unsafeShiftR (fromIntegral a * fromIntegral b) 16 :: Int64)
  fromInteger i = Fixed $ unsafeShiftL (fromInteger i) 16

instance Enum Fixed where
  succ (Fixed a) = Fixed (a + 0x10000)
  pred (Fixed a) = Fixed (a - 0x10000)
  fromEnum = truncate
  toEnum a = Fixed (unsafeShiftL (fromIntegral a) 16)
  enumFrom a           = toFixed `map` enumFrom (fromFixed a)
  enumFromTo a b       = toFixed `map` enumFromTo (fromFixed a) (fromFixed b)
  enumFromThen a b     = toFixed `map` enumFromThen (fromFixed a) (fromFixed b)
  enumFromThenTo a b c = toFixed `map` enumFromThenTo (fromFixed a) (fromFixed b) (fromFixed c)

instance Bounded Fixed where
  minBound = Fixed minBound
  maxBound = Fixed maxBound

instance Fractional Fixed where
  Fixed a / Fixed b  = Fixed $ fromIntegral (unsafeShiftL (fromIntegral a) 16 `div` fromIntegral b :: Int64)
  fromRational a = Fixed $ fromInteger (unsafeShiftL (numerator a) 16 `div` denominator a)

instance Real Fixed where
  toRational (Fixed i) = toInteger i % 65536

instance RealFrac Fixed where
  properFraction (Fixed a)
    | a >= 0 = (fromIntegral (unsafeShiftR a 16), Fixed (a .&. 0xffff))
    | otherwise = (negate $ fromIntegral $ unsafeShiftR (negate a) 16, Fixed $ (a .&. 0xffff) - 0x10000)
  truncate (Fixed a)
    | a >= 0 = fromIntegral (unsafeShiftR a 16)
    | otherwise = negate $ fromIntegral $ unsafeShiftR (negate a) 16
  round (Fixed f)   = fromIntegral $ unsafeShiftR (f + 0x8000) 16
  ceiling (Fixed f) = fromIntegral $ unsafeShiftR (f + 0xffff) 16
  floor (Fixed f)   = fromIntegral $ unsafeShiftR f 16

instance Floating Fixed where
  pi = toFixed pi
  exp = toFixed . exp . fromFixed
  sqrt = toFixed . sqrt . fromFixed
  log = toFixed . log . fromFixed
  a ** b = toFixed $ fromFixed a ** fromFixed b
  logBase a b = toFixed $ logBase (fromFixed a) (fromFixed b)
  sin = toFixed . sin . fromFixed
  tan = toFixed . tan . fromFixed
  cos = toFixed . cos . fromFixed
  asin = toFixed . asin . fromFixed
  atan = toFixed . atan . fromFixed
  acos = toFixed . acos . fromFixed
  sinh = toFixed . sinh . fromFixed
  tanh = toFixed . tanh . fromFixed
  cosh = toFixed . cosh . fromFixed
  asinh = toFixed . asinh . fromFixed
  atanh = toFixed . atanh . fromFixed
  acosh = toFixed . acosh . fromFixed

instance RealFloat Fixed where
  floatRadix  _ = 2
  floatDigits _ = 16
  decodeFloat = decodeFloat . fromFixed
  isInfinite _ = False
  isIEEE _ = False
  atan2 a b = toFixed $ atan2 (fromFixed a) (fromFixed b)
  isDenormalized (Fixed a) = a .&. 0x7fff0000 /= 0
  isNaN _ = False
  isNegativeZero _ = False
  floatRange _ = (15,0)
  encodeFloat i j = toFixed $ encodeFloat i j
  exponent = exponent . fromFixed
  significand = toFixed . significand . fromFixed
  scaleFloat n (Fixed a) = Fixed (shift a n)
