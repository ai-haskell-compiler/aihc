{-# LANGUAGE MagicHash, CPP #-}
{-|
    Module      :  Data.Number.MPFR.Instances.Near
    Description :  Instance declarations
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

  This module defines instances 'Num', 'Real', 'Fractional', 'Floating' and 'RealFrac' of 'MPFR'.
  Operations are rounded with 'RoundMode' 'Near' and computed with maximum precision of two
  operands or with the precision of the operand.
-}

module Data.Number.MPFR.Instances.Near ()
where

import qualified Data.Number.MPFR.Arithmetic as A
import qualified Data.Number.MPFR.Special as S
import Data.Number.MPFR.Misc
import Data.Number.MPFR.Assignment
import Data.Number.MPFR.Comparison
import Data.Number.MPFR.Internal
import Data.Number.MPFR.Conversion
import Data.Number.MPFR.Integer

import Data.Maybe

import Data.Ratio

-- #ifdef INTEGER_SIMPLE
-- --import GHC.Integer.Simple.Internals
-- #endif
-- #ifdef INTEGER_GMP
-- import GHC.Integer.GMP.Internals
-- import qualified GHC.Exts as E
-- #endif

instance Num MPFR where
    d + d'        = A.add Near (maxPrec d d') d d'
    d - d'        = A.sub Near (maxPrec d d') d d'
    d * d'        = A.mul Near (maxPrec d d') d d'
    negate d      = A.neg Near (getPrec d) d
    abs d         = A.absD Near (getPrec d) d
    signum        = fromInt Near minPrec . fromMaybe (-1) . sgn
    fromInteger i =
        fromIntegerA Near (max minPrec $ 1 + bitsInInteger i) i
-- #ifdef INTEGER_SIMPLE
--     fromInteger i =
--         fromIntegerA Near (max minPrec $ 1 + bitsInInteger i) i
-- #endif
-- #ifdef INTEGER_GMP
--     fromInteger (S# i) = fromInt Near minPrec (E.I# i)
--     fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral . abs $ E.I# n * bitsPerIntegerLimb) i
-- #endif

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = A.div Up (maxPrec d d') d d'
    fromRational r = fromInteger n / fromInteger d
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = S.pi Near 53
    exp d        = S.exp Near (getPrec d) d
    log d        = S.log Near (getPrec d) d
    sqrt d       = A.sqrt Near (getPrec d) d
    (**) d d'    = A.pow Near (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = S.sin Near (getPrec d) d
    cos d        = S.cos Near (getPrec d) d
    tan d        = S.tan Near (getPrec d) d
    asin d       = S.asin Near (getPrec d) d
    acos d       = S.acos Near (getPrec d) d
    atan d       = S.atan Near (getPrec d) d
    sinh d       = S.sinh Near (getPrec d) d
    cosh d       = S.cosh Near (getPrec d) d
    tanh d       = S.tanh Near (getPrec d) d
    asinh d      = S.asinh Near (getPrec d) d
    acosh d      = S.acosh Near (getPrec d) d
    atanh d      = S.atanh Near (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Near (getPrec d) d

instance RealFloat MPFR where
    floatRadix _ = 2
    floatDigits = fromInteger . toInteger . getPrec
    floatRange _ = error "floatRange is not defined for MPFR numbers"
    decodeFloat x = (d,e)
      where
      (d,eE) = decompose x
      e = fromInteger (toInteger eE)
    encodeFloat d e =
      (fromInteger d) / ((fromInteger 2)^e) -- TODO: construct it directly
    isNaN (MP _ _ e _) = (e == expNaN)
    isInfinite (MP _ _ e _) = (e == expInf)
    isDenormalized _ = False
    isNegativeZero d@(MP _ _ e _) = (e == expZero && signbit d)
    isIEEE _ = False
    atan2 d1 d2 = S.atan2 Near (maxPrec d1 d2) d1 d2
