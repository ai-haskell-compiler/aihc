{-|
Module      : Math.ExpPairs.RatioInf
Copyright   : (c) Andrew Lelechenko, 2014-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

Rational numbers extended with infinities.
-}

{-# LANGUAGE Safe #-}

module Math.ExpPairs.RatioInf
  ( RatioInf (..)
  , RationalInf
  ) where

import Data.Ratio (Ratio, numerator, denominator)
import Prettyprinter

-- | Extend 'Ratio' @t@ with \( \pm \infty  \) positive and negative
-- infinities.
data RatioInf t
  = InfMinus          -- ^ \( - \infty  \)
  | Finite !(Ratio t) -- ^ Finite value
  | InfPlus           -- ^ \( + \infty  \)
  deriving (Eq, Ord, Show)

-- |Arbitrary-precision rational numbers with positive and negative
-- infinities.
type RationalInf = RatioInf Integer

instance (Integral t, Pretty t) => Pretty (RatioInf t) where
  pretty InfMinus   = pretty "-Inf"
  pretty (Finite x)
    | denominator x == 1 = pretty (numerator x)
    | otherwise          = pretty (numerator x) <+> pretty "/" <+> pretty (denominator x)
  pretty InfPlus    = pretty "+Inf"

instance Integral t => Num (RatioInf t) where
  InfMinus + InfPlus = error "Cannot add up negative and positive infinities"
  InfPlus + InfMinus = error "Cannot add up negative and positive infinities"
  InfMinus + _ = InfMinus
  InfPlus + _  = InfPlus
  _ + InfMinus = InfMinus
  _ + InfPlus  = InfPlus
  (Finite a) + (Finite b) = Finite (a+b)
  {-# SPECIALIZE (+) :: RationalInf -> RationalInf -> RationalInf #-}

  fromInteger = Finite . fromInteger
  {-# SPECIALIZE fromInteger :: Integer -> RationalInf #-}

  signum InfMinus   = Finite (-1)
  signum InfPlus    = Finite 1
  signum (Finite r) = Finite (signum r)
  {-# SPECIALIZE signum :: RationalInf -> RationalInf #-}

  abs InfMinus   = InfPlus
  abs InfPlus    = InfPlus
  abs (Finite r) = Finite (abs r)
  {-# SPECIALIZE abs :: RationalInf -> RationalInf #-}

  negate InfMinus   = InfPlus
  negate InfPlus    = InfMinus
  negate (Finite r) = Finite (negate r)
  {-# SPECIALIZE negate :: RationalInf -> RationalInf #-}

  InfMinus * InfMinus = InfMinus
  InfMinus * InfPlus  = InfMinus
  InfMinus * Finite a = case signum a of
    1  -> InfMinus
    -1 -> InfPlus
    _  -> error "Cannot multiply -infinity by zero"

  InfPlus * InfMinus = InfMinus
  InfPlus * InfPlus  = InfPlus
  InfPlus * Finite a = case signum a of
    1  -> InfPlus
    -1 -> InfMinus
    _  -> error "Cannot multiply +infinity by zero"

  Finite a * InfMinus = case signum a of
    1  -> InfMinus
    -1 -> InfPlus
    _  -> error "Cannot multiply -infinity by zero"

  Finite a * InfPlus = case signum a of
    1  -> InfPlus
    -1 -> InfMinus
    _  -> error "Cannot multiply +infinity by zero"

  Finite a * Finite b = Finite (a * b)

  {-# SPECIALIZE (*) :: RationalInf -> RationalInf -> RationalInf #-}

instance Integral t => Fractional (RatioInf t) where
  fromRational = Finite . fromRational
  {-# SPECIALIZE fromRational :: Rational -> RationalInf #-}

  InfMinus / InfMinus = error "Cannot divide infinity by infinity"
  InfMinus / InfPlus  = error "Cannot divide infinity by infinity"
  InfMinus / Finite a = case signum a of
    1  -> InfMinus
    -1 -> InfPlus
    _  -> error "Cannot divide infinity by zero"

  InfPlus  / InfMinus = error "Cannot divide infinity by infinity"
  InfPlus  / InfPlus  = error "Cannot divide infinity by infinity"
  InfPlus / Finite a  = case signum a of
    1  -> InfPlus
    -1 -> InfMinus
    _  -> error "Cannot divide infinity by zero"

  Finite _ / InfPlus  = Finite 0
  Finite _ / InfMinus = Finite 0

  Finite _ / Finite 0 = error "Cannot divide finite value by zero"
  Finite a / Finite b = Finite (a/b)

  {-# SPECIALIZE (/) :: RationalInf -> RationalInf -> RationalInf #-}

instance Integral t => Real (RatioInf t) where
  toRational (Finite r) = toRational r
  toRational InfPlus    = error "Cannot convert positive infinity into Rational"
  toRational InfMinus   = error "Cannot convert negative infinity into Rational"
  {-# SPECIALIZE toRational :: RationalInf -> Rational #-}
